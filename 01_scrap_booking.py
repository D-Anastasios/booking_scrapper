"""
Author: Anastasios Dadiotis
Date Created: 04/10/2025
Last Modified: 23/10/2025
Description:
    Polite Booking.com scraper using requests + BeautifulSoup, with a browser fallback.
    - Warm-up visit to homepage to set cookies before search
    - Guard-aware retry loop for 202/429 (with exponential backoff + UA jitter)
    - Optional Playwright engine (headless or headful) to fetch fully rendered HTML
    - Handles AWS WAF challenge pages
    - Fetches search results (name, link, location, price, score, reviews)
    - Normalizes price strings and extracts numeric values
    - Saves fetched HTML into ./debug/ (ignored by Git), timestamped
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
HOMEPAGE = "https://www.booking.com/"
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

# ========= 2) Session, UA pool & URL builders =========
UA_POOL = [
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
]

def make_session() -> requests.Session:
    """Configured requests.Session with retries and realistic headers."""
    s = requests.Session()
    s.headers.update({
        "User-Agent": random.choice(UA_POOL),
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language": "en-GB,en;q=0.9",
        "Accept-Encoding": "gzip, deflate, br",
        "Connection": "keep-alive",
        "Upgrade-Insecure-Requests": "1",
        "Cache-Control": "no-cache",
        "Pragma": "no-cache",
        "Referer": HOMEPAGE,
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

# ========= 4) Warm-up + guard-aware fetch (requests engine) =========
def warm_up(session: requests.Session):
    """Visit homepage once to set cookies/consent before search."""
    try:
        print("[WARMUP] Visiting homepage to get cookies…")
        r = session.get(HOMEPAGE, timeout=20, allow_redirects=True)
        print(f"[WARMUP] Status {r.status_code}; redirects={len(r.history)}")
        time.sleep(random.uniform(1.0, 2.0))
    except Exception as e:
        print(f"[WARMUP] Skipped due to error: {e}")

def fetch_html(session: requests.Session, url: str, debug_name: Optional[str]=None) -> Optional[str]:
    """
    GET a URL with guard-aware retry. Saves HTML into ./debug/ if debug_name is provided.
    Retries when status is 202/429 or block detected, with exponential backoff + UA jitter.
    """
    # Ensure cookies first
    warm_up(session)
    
    max_tries = 4
    for attempt in range(1, max_tries + 1):
        # Light header jitter: update Referer and sometimes switch UA
        session.headers["Referer"] = HOMEPAGE
        if attempt > 1:
            session.headers["User-Agent"] = random.choice(UA_POOL)
        
        print(f"[GET] {url} (try {attempt}/{max_tries})")
        resp = session.get(url, timeout=30, allow_redirects=True)
        print(f"[INFO] HTTP status = {resp.status_code}; redirects={len(resp.history)}")
        resp.encoding = "utf-8"
        
        if debug_name:
            # Save first try and each retry separately
            suffix = "" if attempt == 1 else f"_retry{attempt-1}"
            path = DEBUG_DIR / (debug_name.replace(".html", f"{suffix}.html"))
            path.write_text(resp.text, encoding="utf-8")
            print(f"[debug] Saved HTML to {path}")
        
        if resp.status_code == 200 and not looks_like_block(resp.text):
            return resp.text
        
        if resp.status_code in (202, 429) or looks_like_block(resp.text):
            wait = min(12, 2 ** attempt) + random.uniform(0.5, 1.5)
            print(f"[WARN] Got {resp.status_code} or guard page; backing off {wait:.1f}s…")
            time.sleep(wait)
            continue
        
        if resp.status_code != 200:
            print("[WARN] Non-200 status—skipping.")
            return None
    
    print("[WARN] Exhausted retries without a valid page.")
    return None

# ========= 4b) Playwright fallback / engine (supports headful) =========
from contextlib import contextmanager

@contextmanager
def _playwright_sync():
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        print("[PW][ERROR] Playwright is not installed. Run: pip install playwright && playwright install")
        yield None
        return
    pw = sync_playwright().start()
    try:
        yield pw
    finally:
        pw.stop()

COOKIE_BUTTON_SELECTORS = [
    # Common accept-all variants (text and selectors can vary by locale/layout)
    "button:has-text('Accept all')",
    "button:has-text('I agree')",
    "button:has-text('Accept')",
    "[data-testid='uc-accept-all-button']",
    "button[aria-label='Accept all']",
]

def _try_accept_cookies(page, wait_ms: int = 8000):
    """Attempt to accept cookie banner if present."""
    for sel in COOKIE_BUTTON_SELECTORS:
        try:
            el = page.locator(sel)
            if el.count() > 0:
                el.first.click(timeout=wait_ms)
                print(f"[PW] Clicked cookie button: {sel}")
                time.sleep(0.5)
                return
        except Exception:
            continue
    # No cookie button found; that's fine.
    return

def fetch_html_playwright(url: str, debug_name: Optional[str]=None, headless: bool=False, wait_ms: int=60000) -> Optional[str]:
    """
    Use Playwright (Chromium) to fetch a fully-rendered Booking page.
    Handles AWS WAF challenge by waiting for navigation to complete.
    headless=False shows the browser window (headful) so you can watch it.
    Returns page HTML (str) or None on failure.
    """
    print(f"[PW] Launching Chromium (headless={headless})…")
    with _playwright_sync() as pw:
        if pw is None:
            return None
        browser = pw.chromium.launch(
            headless=headless, 
            args=[
                "--disable-blink-features=AutomationControlled",
                "--disable-web-security",
            ]
        )
        context = browser.new_context(
            locale="en-GB",
            user_agent=random.choice(UA_POOL),
            extra_http_headers={
                "Accept-Language": "en-GB,en;q=0.9",
                "Referer": HOMEPAGE,
            },
            viewport={"width": 1920, "height": 1080},
        )
        page = context.new_page()
        try:
            # Warm-up on homepage (cookies/consent)
            print("[PW] Warm-up visit to homepage…")
            page.goto(HOMEPAGE, wait_until="networkidle", timeout=wait_ms)
            time.sleep(2)  # Give cookies time to settle
            _try_accept_cookies(page)
            time.sleep(1)
            
            # Go to target search URL and wait for AWS WAF challenge to complete
            print(f"[PW] Navigating to search URL (this may take a moment for challenge)…")
            
            # Don't use wait_until="domcontentloaded" as it may timeout on challenge page
            # Instead, just navigate and let the page handle redirects
            try:
                page.goto(url, timeout=wait_ms)
            except Exception as nav_error:
                # Sometimes the navigation "fails" but actually succeeds after challenge
                print(f"[PW] Navigation event: {nav_error}")
            
            # Wait for either the challenge to complete or property cards to appear
            print("[PW] Waiting for page to load after challenge…")
            max_wait_seconds = wait_ms // 1000
            for i in range(max_wait_seconds):
                try:
                    # Check if we're past the challenge page
                    current_url = page.url
                    if "searchresults" in current_url or "hotel" in current_url:
                        # Wait for property cards
                        page.wait_for_selector('div[data-testid="property-card"]', timeout=5000)
                        print("[PW] Property cards found!")
                        break
                except Exception:
                    pass
                
                # Check if we're still on a challenge page
                challenge_container = page.locator("#challenge-container")
                if challenge_container.count() > 0:
                    print(f"[PW] Still on challenge page, waiting... ({i+1}s)")
                    time.sleep(1)
                else:
                    # No challenge container, might be on results page
                    time.sleep(1)
                    break
            
            # Give the page a bit more time to fully render
            time.sleep(2)
            
            # Try to accept cookies again if banner appeared after challenge
            _try_accept_cookies(page)
            
            # Wait for network to be idle
            try:
                page.wait_for_load_state("networkidle", timeout=10000)
            except Exception:
                print("[PW] Network didn't become idle, but continuing...")
            
            html = page.content()
            
            # Check if we actually got content
            if 'data-testid="property-card"' not in html and 'challenge-container' in html:
                print("[PW][WARN] Still on challenge page, might need more time or manual intervention")
                if not headless:
                    print("[PW] Browser will stay open for 30s for manual inspection...")
                    time.sleep(30)
            
            if debug_name:
                path = DEBUG_DIR / debug_name
                path.write_text(html, encoding="utf-8")
                print(f"[PW][debug] Saved rendered HTML to {path}")
            
            # Optional: keep the browser a moment when headful so you can see the result
            if not headless:
                print("[PW] Pausing briefly so you can inspect the page...")
                time.sleep(3.0)
            
            return html
        except Exception as e:
            print(f"[PW][WARN] Playwright fetch failed: {e}")
            # If headful, keep browser open to debug
            if not headless:
                print("[PW] Keeping browser open for 15s to debug...")
                time.sleep(15)
            return None
        finally:
            context.close()
            browser.close()

# ========= 5) Scrape orchestration =========
def scrape_search_url(session: requests.Session, url: str, dbg_file: str, engine: str = "requests", headless: bool = True) -> List[dict]:
    """
    Scrape a single search URL; saves HTML to debug/ and returns parsed rows.
    engine: "requests" (default) or "playwright" to force browser fetch.
            If "requests" fails, we automatically fall back to Playwright once.
    headless: only used for Playwright engine (True=headless, False=headful).
    """
    html = None
    if engine == "requests":
        html = fetch_html(session, url, debug_name=dbg_file)
        if html is None:
            # automatic one-time fallback to Playwright
            print("[INFO] Falling back to Playwright for this URL…")
            pw_dbg = dbg_file.replace(".html", "_pw.html")
            html = fetch_html_playwright(url, debug_name=pw_dbg, headless=False)  # headful on fallback to debug
    else:
        pw_dbg = dbg_file if dbg_file.endswith(".html") else f"{dbg_file}.html"
        html = fetch_html_playwright(url, debug_name=pw_dbg, headless=headless)
    
    if not html:
        return []
    
    rows = parse_cards(html)
    print(f"[INFO] Found {len(rows)} cards")
    return rows

def scrape_urls(urls: Iterable[str], min_delay=3, max_delay=6, engine: str = "requests", headless: bool = True) -> List[dict]:
    """
    Scrape multiple URLs sequentially with polite sleeps.
    engine: "requests" | "playwright"
    headless: only used for Playwright engine.
    """
    s = make_session()
    all_rows: List[dict] = []
    urls = list(urls)  # to count
    ts = timestamp()
    
    for i, url in enumerate(urls, start=1):
        print(f"\n[PROGRESS] {i}/{len(urls)} : scraping URL [{engine}] (headless={headless})")
        dbg_file = f"booking_debug_{i:03d}_{ts}.html"
        rows = scrape_search_url(s, url, dbg_file, engine=engine, headless=headless)
        all_rows.extend(rows)
        
        d = random.uniform(min_delay, max_delay)
        print(f"[SLEEP] Sleeping {d:.1f}s to be polite")
        time.sleep(d)
    
    return all_rows

def urls_from_csv(csv_path: str, url_col: str = "url") -> List[str]:
    """Load a column of URLs from a CSV file (UTF-8)."""
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

# ========= 6) Example usage =========
if __name__ == "__main__":
    # Build one URL (adjust as needed)
    url = build_url(city="Lefkada", checkin="2026-08-01", checkout="2026-08-08",
                    adults=6, rooms=3, children=0, currency="EUR", offset=0)
    
    # Choose engine and headless/headful mode:
    #   engine="requests"  -> will auto-fallback to Playwright headful if blocked
    #   engine="playwright" -> uses Playwright directly
    #   headless (for Playwright): False = headful (show browser), True = headless
    engine = "playwright"   # "requests" or "playwright"
    headless = False        # set to False to watch the browser (headful)
    
    rows = scrape_urls([url], min_delay=2, max_delay=4, engine=engine, headless=headless)
    write_csv_timestamped(rows, base_name="booking_lefkada")
    
    # Example B: multiple cities
    # urls = [
    #     build_url(city="Paris", checkin="2025-10-20", checkout="2025-10-22", adults=2, rooms=1, currency="EUR"),
    #     build_url(city="Lyon",  checkin="2025-10-20", checkout="2025-10-22", adults=2, rooms=1, currency="EUR"),
    # ]
    # rows = scrape_urls(urls, min_delay=2, max_delay=5, engine="playwright", headless=False)
    # write_csv_timestamped(rows, base_name="booking_multi_cities")
    
    # Example C: read URLs from a CSV with a 'url' column
    # url_list = urls_from_csv("search_urls.csv", url_col="url")
    # rows = scrape_urls(url_list, min_delay=2, max_delay=5, engine="playwright", headless=False)
    # write_csv_timestamped(rows, base_name="booking_from_csv")