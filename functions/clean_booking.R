clean_booking_file <- function(file) {
  
  folder <- basename(dirname(file))
  location <- str_remove(folder, "_[^_]+$")
  month    <- str_extract(folder, "[^_]+$")
  
  df <- read_csv(file, show_col_types = FALSE) %>%
    mutate(location = location, month = month, source_file = file)
  
  
  #----------------------- Villa features extraction -----------------------#
  messy_cols <- c(
    "a0f53222cd 2",
    "a0f53222cd 3",
    "a0f53222cd 4",
    "fff1944c52 4",
    "a0f53222cd 5",
    "a0f53222cd 6"
  )
  
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
      Booking_link      = `c17271c4d7 href`,
      Profile_image     = `eedb7a060f src`,
      Villa_name        = `b87c397a13`,
      Location_specific = `d823fbbeed`
    )
  
  
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
  
  stars_col <- bullet_cols[1]
  
  df_clean <- df_clean %>%
    mutate(
      stars = str_count(as.character(.data[[stars_col]]), fixed("•"))
    )
  
  
  
  
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