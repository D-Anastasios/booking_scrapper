"""
Author: Anastasios Dadiotis
Date Created: 04/10/2025
Last Modified: 04/10/2025
Description:
    A simple and polite web scraper for Booking.com using `requests` and `BeautifulSoup`.
    The script can:
      • Fetch hotel search results from Booking.com based on city, date, and parameters.
      • Parse hotel names, links, location, prices, and ratings.
      • Normalize and clean price strings, including converting them to numeric format.
      • Read multiple search URLs from a CSV file or list, scrape them all, and save the results.
      • Write cleaned data to a UTF-8-encoded CSV (safe for Excel display).

Usage:
    python 01_scrap_booking.py
"""

import time, random, csv, re, sys
from pathlib import Path
from urllib.parse import urlencode
import unicodedata
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
from bs4 import BeautifulSoup
from typing import List, Iterable, Optional

# ========= 0) Utilities for price/encoding =========
def clean_text(s: Optional[str]) -> Optional[str]:
    """Normalize unicode, convert NBSPs to spaces, trim."""
    if not s:
        return s
    s = unicodedata.normalize("NFKC", s)
    # replace non-breaking spaces (U+00A0, U+202F) with regular space
    s = s.replace("\u00A0", " ").replace("\u202F", " ")
    return s.strip()

def price_to_float(s: Optional[str]) -> Optional[float]:
    """Extract a numeric price (supports EU/US separators)."""
    if not s:
        return None
    s = clean_text(s)

    # Keep only digits and separators
    num = re.sub(r"[^\d.,]", "", s)
    # If EU-style with both . and , and comma as decimal
    if num.count(",") == 1 and num.count(".") >= 1 and num.rfind(",") > num.rfind("."):
        num = num.replace(".", "").replace(",", ".")
    else:
        # Otherwise drop thousands commas
        num = num.replace(",", "")
    try:
        return float(num)
    except ValueError:
        return None

# ========= 1) Session & URL builders =========
def make_session() -> requests.Session:
    s = requests.Session()
    s.headers.update({
        "User-Agent": (
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/120.0.0.0 Safari/537.36"
        ),
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language": "en-GB,en;q=0.9",
        "Connection": "keep-alive",
        "Upgrade-Insecure-Requests": "1",
    })
    retry = Retry(
        total=5,
        backoff_factor=1.0,
        status_forcelist=[429, 500, 502, 503, 504],
        allowed_methods=["GET"],
        raise_on_status=False,
    )
    s.mount("https://", HTTPAdapter(max_retries=retry))
    s.mount("http://", HTTPAdapter(max_retries=retry))
    return s

BASE_URL = "https://www.booking.com/searchresults.html"

def build_url(city="Lefkada", checkin="2025-10-04", checkout="2025-10-05",
              adults=2, rooms=1, children=0, currency="EUR", offset=0) -> str:
    params = {
        "ss": city,
        "checkin": checkin,
        "checkout": checkout,
        "group_adults": adults,
        "no_rooms": rooms,
        "group_children": children,
        "selected_currency": currency,
        "offset": offset,     # 0, 25, 50...
        "order": "price",     # optional
        "lang": "en-gb",
    }
    return f"{BASE_URL}?{urlencode(params)}"

# ========= 2) Parsing =========
def looks_like_block(html_text: str) -> bool:
    t = html_text.lower()
    return any(
        k in t for k in [
            "unusual traffic", "verify you are a human", "enable javascript", "captcha"
        ]
    )

def safe_text(el):
    return el.get_text(strip=True) if el else None

def parse_cards(html: str):
    soup = BeautifulSoup(html, "lxml")
    cards = soup.select('div[data-testid="property-card"]')
    out = []
    for c in cards:
        title_el = c.select_one('div[data-testid="title"]')
        link_el  = c.select_one('a[data-testid="title-link"]')
        addr_el  = c.select_one('span[data-testid="address"]')
        price_el = (
            c.select_one('span[data-testid="price-and-discounted-price"]')
            or c.select_one('span[data-testid="price-and-discounted-price"] span')
        )
        score_box = c.select_one('div[data-testid="review-score"]')

        title = clean_text(safe_text(title_el))
        rel   = link_el.get("href") if link_el else None
        url   = f"https://www.booking.com{rel}" if rel and rel.startswith("/") else rel
        addr  = clean_text(safe_text(addr_el))
        price_raw = safe_text(price_el)
        price_text = clean_text(price_raw)
        price_eur  = price_to_float(price_raw)

        score = None
        reviews_raw = None
        if score_box:
            first_div = score_box.select_one("div")
            score = clean_text(safe_text(first_div))
            reviews_raw = clean_text(score_box.get_text(" ", strip=True))

        out.append({
            "name": title,
            "url": url,
            "location": addr,
            "price_text": price_text,  # now human-friendly (no NBSP mojibake)
            "price_eur": price_eur,    # numeric for analysis
            "score": score,
            "reviews_raw": reviews_raw
        })
    return out

# ========= 3) Scrape helpers (one URL or many) =========
def fetch_html(session: requests.Session, url: str, save_debug: Optional[Path]=None) -> Optional[str]:
    print(f"[GET] {url}")
    resp = session.get(url, timeout=30)
    print(f"[INFO] HTTP status = {resp.status_code}")
    # Ensure proper decoding
    resp.encoding = "utf-8"
    if save_debug:
        save_debug.write_text(resp.text, encoding="utf-8")
        print(f"[debug] Saved HTML to {save_debug}")
    if resp.status_code != 200:
        print("[WARN] Non-200 status—skipping.")
        return None
    if looks_like_block(resp.text):
        print("[WARN] Page looks like a bot/guard page—skipping.")
        return None
    return resp.text

def scrape_search_url(session: requests.Session, url: str) -> List[dict]:
    html = fetch_html(session, url, save_debug=Path("booking_debug.html"))
    if not html:
        return []
    rows = parse_cards(html)
    print(f"[INFO] Found {len(rows)} cards")
    return rows

def scrape_urls(urls: Iterable[str], min_delay=3, max_delay=6) -> List[dict]:
    s = make_session()
    all_rows: List[dict] = []
    for i, url in enumerate(urls, start=1):
        print(f"\n[PROGRESS] {i}/{len(list(urls)) if hasattr(urls, '__len__') else '?'} : scraping URL")
        rows = scrape_search_url(s, url)
        all_rows.extend(rows)
        d = random.uniform(min_delay, max_delay)
        print(f"[SLEEP] Sleeping {d:.1f}s to be polite")
        time.sleep(d)
    return all_rows

def urls_from_csv(csv_path: str, url_col: str = "url") -> List[str]:
    urls = []
    with open(csv_path, "r", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        if url_col not in reader.fieldnames:
            raise ValueError(f"Column '{url_col}' not found in {csv_path}. Columns: {reader.fieldnames}")
        for row in reader:
            u = row.get(url_col)
            if u:
                urls.append(u.strip())
    print(f"[INFO] Loaded {len(urls)} URLs from {csv_path}")
    return urls

def write_csv(rows: List[dict], out_path: str):
    if not rows:
        print("[INFO] No rows to write.")
        return
    keys = ["name", "url", "location", "price_text", "price_eur", "score", "reviews_raw"]
    # utf-8-sig helps Excel display € properly
    with open(out_path, "w", newline="", encoding="utf-8-sig") as f:
        w = csv.DictWriter(f, fieldnames=keys)
        w.writeheader()
        w.writerows(rows)
    print(f"[DONE] Wrote {len(rows)} rows -> {out_path}")

# ========= 4) Example usages =========
if __name__ == "__main__":
    # A) Single built URL (like your current flow)
    url = build_url(city="Lefkada", checkin="2025-10-04", checkout="2025-10-05",
                    adults=6, rooms=3, children=0, currency="EUR", offset=0)
    rows = scrape_urls([url], min_delay=2, max_delay=4)
    write_csv(rows, "booking_lefkada.csv")

    # B) OR: Pass an explicit list of URLs
    # urls = [
    #     build_url(city="Paris",  checkin="2025-10-20", checkout="2025-10-22", adults=2, rooms=1),
    #     build_url(city="Lyon",   checkin="2025-10-20", checkout="2025-10-22", adults=2, rooms=1),
    # ]
    # rows = scrape_urls(urls, min_delay=2, max_delay=5)
    # write_csv(rows, "booking_multi_cities.csv")

    # C) OR: Load URLs from a CSV with a 'url' column
    # url_list = urls_from_csv("search_urls.csv", url_col="url")
    # rows = scrape_urls(url_list, min_delay=2, max_delay=5)
    # write_csv(rows, "booking_from_csv.csv")
