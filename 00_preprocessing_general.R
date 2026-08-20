library(dplyr)
library(stringr)
library(purrr)
library(future)
library(furrr)
library(progressr)
library(tidyverse)

# choose workers (leave 2 core for system)
plan(multisession, workers = max(1, parallel::detectCores() - 2))

handlers(global = TRUE)
handlers("txtprogressbar")


# create a function to clean individual booking files

clean_booking_file <- function(file) {
  
  folder <- basename(dirname(file))
  location <- str_remove(folder, "_[^_]+$")
  month    <- str_extract(folder, "[^_]+$")

    df <- read_csv(file, show_col_types = FALSE) %>%
      mutate(location = location, month = month, source_file = file)
    
    
    #----------------------- Villa features extraction -----------------------#
    
    # Tokens that strongly indicate structured specs (not just a title)
    spec_tokens <- regex("μπάνι|κουζίνα|κρεβάτ|m²|τ\\.?\\s*μ", ignore_case = TRUE)
    
    # Bedroom token (weak on its own because it appears in titles)
    bedroom_token <- regex("υπνοδωμάτι|υπνοδωματί", ignore_case = TRUE)
    
    # Helper: does a column contain any match for a regex?
    col_has <- function(x, pattern) {
      x <- str_squish(as.character(x))
      any(str_detect(x, pattern), na.rm = TRUE)
    }
    
    # 1) Strong spec columns: contain bathroom/kitchen/beds/sqm tokens
    spec_cols <- names(df)[vapply(df, col_has, logical(1), pattern = spec_tokens)]
    
    # 2) Bedroom-only columns that are also "spec-like":
    # contain bedrooms AND ALSO contain at least one strong spec token
    bedroom_cols <- names(df)[vapply(df, col_has, logical(1), pattern = bedroom_token)]
    bedroom_spec_cols <- bedroom_cols[
      vapply(df[bedroom_cols], col_has, logical(1), pattern = spec_tokens)
    ]
    
    # 3) Final messy/spec columns to pivot
    
    messy_cols <- union(spec_cols, bedroom_spec_cols)
    
    protected_cols <- c(
      "c17271c4d7 href",  # Booking_link
      "eedb7a060f src",   # Profile_image
      "b87c397a13",       # Villa_name
      "d823fbbeed"        # Location_specific
    )
    
    messy_cols <- setdiff(messy_cols, protected_cols)
    
    messy_cols
    features_wide <- df %>%
      transmute(row_id = row_number(), across(all_of(messy_cols))) %>%
      pivot_longer(
        cols = all_of(messy_cols),
        names_to = "raw_col",
        values_to = "raw_text"
      ) %>%
      mutate(
        raw_text = str_squish(as.character(raw_text)),
        raw_text = na_if(raw_text, "")
      ) %>%
      filter(!is.na(raw_text)) %>%
      mutate(
        type = case_when(
          str_detect(raw_text, regex("υπνοδωμάτι", ignore_case = TRUE)) ~ "bedrooms",
          str_detect(raw_text, regex("μπάνι", ignore_case = TRUE))      ~ "bathrooms",
          str_detect(raw_text, regex("κουζίνα", ignore_case = TRUE))    ~ "kitchen",
          str_detect(raw_text, regex("κρεβάτ", ignore_case = TRUE))     ~ "beds",
          str_detect(raw_text, regex("m²|τ\\.?\\s*μ", ignore_case = TRUE)) ~ "sqm",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(type)) %>%
      mutate(
        # beds: keep ONLY the total outside parentheses
        text_no_paren = if_else(
          type == "beds",
          str_remove(raw_text, "\\s*\\(.*\\)$"),
          raw_text
        ),
        value = parse_number(text_no_paren, locale = locale(decimal_mark = ","))
      ) %>%
      filter(!is.na(value)) %>%
      group_by(row_id, type) %>%
      summarise(value = max(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = type,
        values_from = value
      )
    
    df_clean <- df %>%
      mutate(row_id = row_number()) %>%
      left_join(features_wide, by = "row_id") %>%
      select(-row_id)
    
    df_clean <- df_clean %>%
      select(-all_of(messy_cols))
    
    #----------------------- Standardize column names -----------------------#
    
    
    
    df_clean <- df_clean %>%
      rename(
        Booking_link  = `c17271c4d7 href`,
        Profile_image = `eedb7a060f src`,
        Villa_name    = `b87c397a13`
      ) %>%
      { if ("d823fbbeed" %in% names(.)) rename(., Location_specific = `d823fbbeed`)
        else mutate(., Location_specific = NA_character_) }
    
    #----------------------- Pool variable extraction -----------------------#
    # 1) Identify which columns contain pool mentions anywhere
    pool_search_pattern <- regex("pool|πισινα|πισίνα", ignore_case = TRUE)
    
    pool_cols <- names(df_clean)[
      sapply(df_clean, function(x)
        any(str_detect(str_to_lower(as.character(x)), pool_search_pattern), na.rm = TRUE)
      )
    ]
    
    pool_cols  # inspect which columns matched
    
    # 2) Create pool variable based on ANY of these columns per row
    df_clean <- df_clean %>%
      mutate(
        pool = if_else(
          rowSums(
            across(
              all_of(pool_cols),
              ~ str_detect(str_to_lower(as.character(.x)), pool_search_pattern)
            ),
            na.rm = TRUE
          ) > 0,
          "pool",
          "no_pool"
        )
      )
    
    #----------------------- Rating extraction -----------------------#
    bullet_cols <- names(df_clean)[
      sapply(df_clean, function(x)
        any(str_detect(as.character(x), fixed("•")), na.rm = TRUE)
      )
    ]
    
    bullet_cols
    
    
    # Safe stars extraction - guard against empty bullet_cols
    if (length(bullet_cols) > 0) {
      stars_col <- bullet_cols[1]
      df_clean <- df_clean %>%
        mutate(stars = str_count(as.character(.data[[stars_col]]), fixed("•")))
    } else {
      df_clean <- df_clean %>%
        mutate(stars = NA_integer_)
    }
    
    
    
    df_clean <- df_clean %>%
      mutate(
        rating = parse_number(
          as.character(`f63b14ab7a`),
          locale = locale(decimal_mark = ",")
        )
      )
    
    df_clean <- df_clean %>% select(-`f63b14ab7a`)
    
    # Rename rating category column
    df_clean <- df_clean %>%
      rename(rating_cat = `f63b14ab7a 2`)
    
    
    #----------------------- Comments extraction -----------------------#
    comments_pattern <- regex("σχόλι", ignore_case = TRUE)
    
    comment_cols <- names(df_clean)[
      sapply(df_clean, function(x)
        any(str_detect(as.character(x), comments_pattern), na.rm = TRUE)
      )
    ]
    
    comment_cols
    
    comments_col <- comment_cols[1]
    
    df_clean <- df_clean %>%
      mutate(
        number_comments = parse_number(
          as.character(.data[[comments_col]]),
          locale = locale(decimal_mark = ",")
        )
      )
    
    df_clean <- df_clean %>%
      select(-all_of(comments_col))
    
    #----------------------- Comfort extraction -----------------------#
    comfort_pattern <- regex("Άνεση", ignore_case = TRUE)
    
    comfort_cols <- names(df_clean)[
      sapply(df_clean, function(x)
        any(str_detect(as.character(x), comfort_pattern), na.rm = TRUE)
      )
    ]
    
    comfort_cols
    
    
    comfort_col <- comfort_cols[1]
    
    df_clean <- df_clean %>%
      mutate(
        comfort_rate = parse_number(
          as.character(.data[[comfort_col]]),
          locale = locale(decimal_mark = ",")
        )
      )
    
    
    #----------------------- Available dates extraction -----------------------#
    avail_cols <- names(df_clean)[
      sapply(df_clean, function(x)
        any(str_detect(as.character(x), "Δευτ\\."), na.rm = TRUE)
      )
    ]
    
    avail_cols
    
    avail_col <- avail_cols[1]
    
    df_clean <- df_clean %>%
      rename(available_dates = all_of(avail_col))
    
    #----------------------- Distance from center extraction -----------------------#
    
    # 1) Find candidate column (may not exist for all datasets)
    center_cols <- names(df_clean)[
      sapply(df_clean, function(x)
        any(str_detect(as.character(x), fixed("από το κέντρο")), na.rm = TRUE)
      )
    ]
    
    # 2) Create distance_from_center_km (numeric, in km)
    if (length(center_cols) == 0) {
      
      df_clean <- df_clean %>%
        mutate(distance_from_center_km = NA_real_)
      
    } else {
      
      center_col <- center_cols[1]
      
      df_clean <- df_clean %>%
        mutate(
          .center_text = as.character(.data[[center_col]]),
          .center_text = str_squish(.center_text),
          
          distance_from_center_km = case_when(
            is.na(.center_text) ~ NA_real_,
            
            # already in km (χλμ)
            str_detect(.center_text, "χλμ\\.?") ~ parse_number(
              .center_text, locale = locale(decimal_mark = ",")
            ),
            
            # meters (μ.) -> convert to km
            str_detect(.center_text, "(^|\\s)μ\\.?($|\\s)") ~ parse_number(
              .center_text, locale = locale(decimal_mark = ",")
            ) / 1000,
            
            TRUE ~ NA_real_
          )
        ) %>%
        select(-.center_text)
    }
    
    #----------------------- Distance from beach extraction -----------------------#
    
    # 1) Find column containing "από την παραλία"
    beach_cols <- names(df_clean)[
      sapply(df_clean, function(x)
        any(str_detect(as.character(x), fixed("από την παραλία")), na.rm = TRUE)
      )
    ]
    
    # 2) Create distance_from_beach_km
    if (length(beach_cols) == 0) {
      
      df_clean <- df_clean %>%
        mutate(distance_from_beach_km = NA_real_)
      
    } else {
      
      beach_col <- beach_cols[1]
      
      df_clean <- df_clean %>%
        mutate(
          .beach_text = as.character(.data[[beach_col]]),
          .beach_text = str_squish(.beach_text),
          
          distance_from_beach_km = case_when(
            is.na(.beach_text) ~ NA_real_,
            
            # already in km
            str_detect(.beach_text, "χλμ\\.?") ~ parse_number(
              .beach_text, locale = locale(decimal_mark = ",")
            ),
            
            # meters -> km
            str_detect(.beach_text, "(^|\\s)μ\\.?($|\\s)") ~ parse_number(
              .beach_text, locale = locale(decimal_mark = ",")
            ) / 1000,
            
            TRUE ~ NA_real_
          )
        ) %>%
        select(-.beach_text)
    }
    
    #----------------------- Price extraction -----------------------#
    
    # 1) Find the column that contains price text
    price_pattern <- regex("Αρχική τιμή|Τρέχουσα τιμή|Τιμή", ignore_case = TRUE)
    
    price_cols <- names(df_clean)[
      sapply(df_clean, function(x)
        any(str_detect(as.character(x), price_pattern), na.rm = TRUE)
      )
    ]
    
    price_cols  # inspect
    
    # Use the first match (usually the right one)
    price_col <- price_cols[1]
    
    # Locale: dot as thousands separator, comma as decimal (Greek formatting)
    eu_loc <- locale(grouping_mark = ".", decimal_mark = ",")
    
    df_clean <- df_clean %>%
      mutate(
        .price_text = str_squish(as.character(.data[[price_col]])),
        
        # Extract original price ONLY when "Αρχική τιμή" exists
        original_price = case_when(
          str_detect(.price_text, regex("Αρχική τιμή", ignore_case = TRUE)) ~
            parse_number(
              str_match(.price_text, regex("Αρχική τιμή\\s*€\\s*([0-9\\.,]+)", ignore_case = TRUE))[, 2],
              locale = eu_loc
            ),
          TRUE ~ NA_real_
        ),
        
        # Extract current price:
        # - if "Τρέχουσα τιμή" exists -> use it
        # - else if "Τιμή" exists -> use it
        price = case_when(
          str_detect(.price_text, regex("Τρέχουσα τιμή", ignore_case = TRUE)) ~
            parse_number(
              str_match(.price_text, regex("Τρέχουσα τιμή\\s*€\\s*([0-9\\.,]+)", ignore_case = TRUE))[, 2],
              locale = eu_loc
            ),
          str_detect(.price_text, regex("\\bΤιμή\\b", ignore_case = TRUE)) ~
            parse_number(
              str_match(.price_text, regex("\\bΤιμή\\b\\s*€\\s*([0-9\\.,]+)", ignore_case = TRUE))[, 2],
              locale = eu_loc
            ),
          TRUE ~ NA_real_
        ),
        
        # Discount rate only when both exist
        discount_rate = if_else(
          !is.na(original_price) & !is.na(price) & original_price > 0,
          (original_price - price) / original_price,
          NA_real_
        )
      ) %>%
      select(-.price_text)
    
    df_clean <- df_clean %>% mutate(discount_pct = 100 * discount_rate)
    
    #----------------------- Whole villa for rent extraction -----------------------#
    needle <- "Oλόκληρη βίλα"
    
    whole_villa_cols <- names(df_clean)[
      vapply(df_clean, function(x) {
        x <- str_squish(as.character(x))      # normalize whitespace
        any(str_detect(x, fixed(needle)), na.rm = TRUE)
      }, logical(1))
    ]
    
    whole_villa_cols
    
    
    if (length(whole_villa_cols) > 0) {
      df_clean <- df_clean %>%
        rename(Whole_villa_for_rent = !!whole_villa_cols[1])
    }
    
    #----------------------- Cancellation policy extraction -----------------------#
    
    cancel_cols <- names(df_clean)[
      vapply(df_clean, function(x) {
        x <- str_squish(as.character(x))
        any(str_detect(x, regex("ακύρωση", ignore_case = TRUE)), na.rm = TRUE)
      }, logical(1))
    ]
    
    cancel_cols
    
    if (length(cancel_cols) > 0) {
      df_clean <- df_clean %>%
        rename(cancellation_policy = all_of(cancel_cols[1]))
    } else {
      message("No column containing 'ακύρωση' was found.")
    }
    
    
    df_clean <- df_clean %>%
      select(
        -matches("^[a-f0-9]{8,}"),
        -matches("^[a-f0-9]{8,}\\s"),
        -matches("href")
      )
    
    df_clean
}

# ----------------------- Error logging helper -----------------------#

log_error <- function(file, error, log_dir = "Data") {
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(log_dir, "cleaning_errors.txt")
  msg <- paste(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "\nFILE:", file,
    "\nERROR:", conditionMessage(error),
    "\n------------------------\n"
  )
  cat(msg, file = log_file, append = TRUE)
}

# ----------------------- Safe cleaning wrapper -----------------------#


safe_clean_and_save_result <- function(file) {
  tryCatch(
    {
      df_clean <- clean_booking_file(file)
      
      out_name <- paste0(
        tools::file_path_sans_ext(basename(file)),
        "_clean.csv"
      )
      out_path <- file.path(dirname(file), out_name)
      
      write_csv(df_clean, out_path)
      
      list(
        ok = TRUE,
        file = file,
        out_path = out_path,
        error_message = NA_character_,
        data = df_clean
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        file = file,
        out_path = NA_character_,
        error_message = conditionMessage(e),
        data = NULL
      )
    }
  )
}


# ----------------------- Process all files -----------------------#



data_root <- file.path("../Data/scrapping2_6 adults_villas_privatepool")



files <- list.files(
  path = data_root,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

files <- files[
  !str_detect(files, "_clean\\.csv$") &   # exclude cleaned outputs
    !str_detect(files, "/~\\$")             # exclude Excel temp files if any
]


results <- with_progress({
  p <- progressor(along = files)
  
  future_map(
    files,
    function(f) { p(); safe_clean_and_save_result(f) },
    .options = furrr_options(seed = TRUE)
  )
})

data_booking_clean <- bind_rows(
  purrr::map(results, "data")
)

errors_df <- tibble(
  file = purrr::map_chr(results, "file"),
  ok   = purrr::map_lgl(results, "ok"),
  out_path = purrr::map_chr(results, "out_path"),
  error_message = purrr::map_chr(results, "error_message")
) %>%
  filter(!ok)

errors_df <- errors_df %>%
  mutate(
    folder = basename(dirname(file)),
    location = str_remove(folder, "_[^_]+$"),
    month = str_extract(folder, "[^_]+$")
  ) %>%
  select(-folder)


#------------------------------ Checks ------------------------------#

count_df = data_booking_clean %>% count(location, month) %>% arrange(location, month)

# Save final cleaned data
out_final_path <- file.path(data_root, "data_booking_clean_all.csv")
write_csv(data_booking_clean, out_final_path)

# Save errors log
if (nrow(errors_df) > 0) {
  log_errors_path <- file.path(data_root, "cleaning_errors_summary.csv")
  write_csv(errors_df, log_errors_path)
}


#------------------------------ only Paros------------------------------#


paros_test_files <- list.files(
  path = "/Users/anastasiosdadiotes/Documents/erato_project/Data/scrapping2_6 adults_villas_privatepool/paros",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Exclude already cleaned files
paros_test_files <- paros_test_files[!str_detect(paros_test_files, "_clean\\.csv$")]

paros_test_files

# Check what files were found
paros_files = paros_test_files

# Run on each and capture results
paros_results <- map(paros_files, safe_clean_and_save_result)

# See which failed and why
map(paros_results, ~ list(file = .x$file, ok = .x$ok, error = .x$error_message))

# 1) Remove bad mykonos rows
data_booking_clean_all <- data_booking_clean_all %>%
  filter(!(location == "mykonos" & is.na(month)))

# 2) Clean mykonos files
mykonos_files <- list.files(
  "/Users/anastasiosdadiotes/Documents/erato_project/Data/scrapping2_6 adults_villas_privatepool/mykonos",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
) %>% .[!str_detect(., "_clean\\.csv$")]

mykonos_results <- map(mykonos_files, safe_clean_and_save_result)

# Check errors before binding
map(mykonos_results, ~ list(file = basename(.x$file), ok = .x$ok, error = .x$error_message))