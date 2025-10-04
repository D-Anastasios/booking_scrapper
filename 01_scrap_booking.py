"""
Author: Anastasios Dadiotis
Date Created: 04/10/2025
Last Modified: 04/10/2025
Description:
    Polite Booking.com scraper using requests + BeautifulSoup.
    - Fetches search results (name, link, location, price, score, reviews)
    - Normalizes price strings and extracts numeric values
    - Saves fetched HTML into ./debug/ (ignored by Git)
    - Supports multiple URLs (list or CSV input)
    - Writes results to timestamped UTF-8 CSV (Excel-friendly)

Usage:
    python 01_scrap_booking.py
"""

import time, random, csv, re, sys, unicodedata
from pathlib import Path
from urllib.parse import urlencode
from datetime import datetime
from typing import List, Iterable, Optional

import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
from bs4 import BeautifulSoup

# ========= 0) Globals & timestamp helpers =========
BASE_URL = "https://www.booking.com/searchresults.html"
DEBUG_DIR = Path("debug")
DEBUG_DIR.mkdir(exist_ok=True)

def timestamp() -> str:
    """Return a compact timestamp string like 20251004_183015."""
    return datetime.now().strftime("%Y%m%d_%H%M%S")

# ========= 1) Utilities for price/encoding =========
def clean_text(s: Optional[str]) -> Optional[str]:
    """Normalize unicode, replace NBSPs, strip."""
    if not s:
        return s
    s = unicodedata.normalize("NFKC", s)
    s = s.replace("\u00A0", " ").replace("\u202F", " ")
    return s.strip()

def price_to_float(s: Optional[str]) -> Optional[float]:
    """Extract a numeric price (handles EU and US separators)."""
    if not s:
        return None
    s = clean_text(s)
    num = re.sub(r"[^\d.,]", "", s)
    # EU style like "1.234,56" → "1234.56"
    if num.count(",") == 1 and num.count(".") >= 1 and num.rfind(",") > num.rfind("."):
        num = num.replace(".", "").replace(",", ".")
    else:
        num = num.replace(",", "")
    try:
        return float(num)
    except ValueError:
        return None

# ========= 2) Session & URL builders =========
def make_session() -> requests.Session:
    """Configured requests.Session with retries and realistic headers."""
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

def build_url(city="Lefkada", checkin="2025-10-04", checkout="2025-10-05",
              adults=2, rooms=1, children=0, currency="EUR", offset=0) -> str:
    """Construct a Booking.com search URL with common params."""
    params = {
        "ss": city,
        "checkin": checkin,
        "checkout": checkout,
        "group_adults": adults,
        "no_rooms": rooms,
        "group_children": children,
        "selected_currency": currency,
        "offset": offset,      # 0, 25, 50 ...
        "order": "price",      # optional
        "lang": "en-gb",
    }
    return f"{BASE_URL}?{urlencode(params)}"

# ========= 3) Parsing =========
def looks_like_block(html_text: str) -> bool:
    """Detect basic signs of bot/guard/captcha pages."""
    t = html_text.lower()
    return any(k in t for k in ["unusual traffic", "verify you are a human", "enable javascript", "captcha"])

def safe_text(el):
    """Safely extract text from a BeautifulSoup element."""
    return el.get_text(strip=True) if el else None

def parse_cards(html: str) -> List[dict]:
    """Parse hotel cards and return rows of extracted fields."""
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
        price_raw  = safe_text(price_el)
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
            "price_text": price_text,
            "price_eur": price_eur,
            "score": score,
            "reviews_raw": reviews_raw,
        })
    return out

# ========= 4) Scrape helpers =========
def fetch_html(session: requests.Session, url: str, debug_name: Optional[str]=None) -> Optional[str]:
    """
    GET a URL, save HTML into ./debug/ if debug_name is provided, return text or None.
    """
    print(f"[GET] {url}")
    resp = session.get(url, timeout=30)
    print(f"[INFO] HTTP status = {resp.status_code}")
    resp.encoding = "utf-8"

    if debug_name:
        path = DEBUG_DIR / debug_name
        path.write_text(resp.text, encoding="utf-8")
        print(f"[debug] Saved HTML to {path}")

    if resp.status_code != 200:
        print("[WARN] Non-200 status—skipping.")
        return None
    if looks_like_block(resp.text):
        print("[WARN] Page looks like a bot/guard page—skipping.")
        return None
    return resp.text

def scrape_search_url(session: requests.Session, url: str, dbg_file: str) -> List[dict]:
    """Scrape a single search URL; saves HTML to debug/ and returns parsed rows."""
    html = fetch_html(session, url, debug_name=dbg_file)
    if not html:
        return []
    rows = parse_cards(html)
    print(f"[INFO] Found {len(rows)} cards")
    return rows

def scrape_urls(urls: Iterable[str], min_delay=3, max_delay=6) -> List[dict]:
    """
    Scrape multiple URLs sequentially with polite sleeps.
    Saves timestamped debug HTML per URL to ./debug/.
    """
    s = make_session()
    all_rows: List[dict] = []
    urls = list(urls)  # to count
    ts = timestamp()
    for i, url in enumerate(urls, start=1):
        print(f"\n[PROGRESS] {i}/{len(urls)} : scraping URL")
        dbg_file = f"booking_debug_{i:03d}_{ts}.html"
        rows = scrape_search_url(s, url, dbg_file)
        all_rows.extend(rows)
        d = random.uniform(min_delay, max_delay)
        print(f"[SLEEP] Sleeping {d:.1f}s to be polite")
        time.sleep(d)
    return all_rows

def urls_from_csv(csv_path: str, url_col: str = "url") -> List[str]:
    """
    Load a column of URLs from a CSV file (UTF-8).
    """
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

def write_csv_timestamped(rows: List[dict], base_name: str) -> str:
    """
    Write rows to a timestamped CSV in UTF-8 with BOM.
    Returns the output path.
    """
    if not rows:
        print("[INFO] No rows to write.")
        return ""
    keys = ["name", "url", "location", "price_text", "price_eur", "score", "reviews_raw"]
    out_path = f"{base_name}_{timestamp()}.csv"
    with open(out_path, "w", newline="", encoding="utf-8-sig") as f:
        w = csv.DictWriter(f, fieldnames=keys)
        w.writeheader()
        w.writerows(rows)
    print(f"[DONE] Wrote {len(rows)} rows -> {out_path}")
    return out_path

# ========= 5) Example usage =========
if __name__ == "__main__":
    # Example A: single built URL
    url = build_url(city="Lefkada", checkin="2025-10-04", checkout="2025-10-05",
                    adults=6, rooms=3, children=0, currency="EUR", offset=0)
    rows = scrape_urls([url], min_delay=2, max_delay=4)
    write_csv_timestamped(rows, base_name="booking_lefkada")

    # Example B: multiple cities (uncomment to use)
    # urls = [
    #     build_url(city="Paris", checkin="2025-10-20", checkout="2025-10-22", adults=2, rooms=1, currency="EUR"),
    #     build_url(city="Lyon",  checkin="2025-10-20", checkout="2025-10-22", adults=2, rooms=1, currency="EUR"),
    # ]
    # rows = scrape_urls(urls, min_delay=2, max_delay=5)
    # write_csv_timestamped(rows, base_name="booking_multi_cities")

    # Example C: read URLs from a CSV with a 'url' column (uncomment to use)
    # url_list = urls_from_csv("search_urls.csv", url_col="url")
    # rows = scrape_urls(url_list, min_delay=2, max_delay=5)
    # write_csv_timestamped(rows, base_name="booking_from_csv")