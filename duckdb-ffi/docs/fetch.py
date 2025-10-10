#!/usr/bin/env python3
import re
from pathlib import Path
from urllib.parse import urljoin, urlparse

import requests
from bs4 import BeautifulSoup

BASE = "https://duckdb.org"
SITEMAP_URL = f"{BASE}/sitemap.html"
OUT = Path("c_api.md")

def http_get(url: str) -> str:
    r = requests.get(url, timeout=20)
    r.raise_for_status()
    r.encoding = r.apparent_encoding or "utf-8"
    return r.text

def clean_html_to_text(content: str) -> str:
    soup = BeautifulSoup(content, "html.parser")
    text = soup.get_text()
    text = re.sub(r'\r\n?', '\n', text)
    text = re.sub(r'[ \t]+\n', '\n', text)
    text = re.sub(r'\n{3,}', '\n\n', text)
    return text.strip() + "\n"

def ensure_md_url(u: str) -> str:
    parsed = urlparse(u)
    if parsed.path.endswith(".md"):
        return u
    return u.rstrip("/") + ".md"

def is_c_client_block(ul) -> bool:
    """True iff this UL looks like the C client menu: links under /docs/.../clients/c/"""
    links = [a.get("href", "").strip() for a in ul.select("a[href]")]
    return (
        len(links) > 0
        and all("/clients/c/" in href for href in links)
    )

def main():
    sitemap_html = http_get(SITEMAP_URL)
    soup = BeautifulSoup(sitemap_html, "html.parser")

    # Find all entry sections with submenus, then choose the UL whose links are /clients/c/
    c_ul = None
    for li in soup.select("li.entry-menu.hasSub"):
        ul = li.find_next_sibling("ul")
        if ul and is_c_client_block(ul):
            c_ul = ul
            break
    if not c_ul:
        # Fallback heuristic: directly scan all ULs for /clients/c/ pattern
        for ul in soup.find_all("ul"):
            if is_c_client_block(ul):
                c_ul = ul
                break
    if not c_ul:
        raise RuntimeError("Could not locate the C API block (clients/c) in sitemap")

    # Extract absolute URLs from that UL
    hrefs = [urljoin(BASE, a["href"].strip()) for a in c_ul.select("a[href]")]
    # Deduplicate while preserving order
    seen, urls = set(), []
    for u in hrefs:
        if u not in seen:
            seen.add(u)
            urls.append(u)

    parts = []
    for u in urls:
        md_url = ensure_md_url(u)
        raw = http_get(md_url)  # .md only
        cleaned = clean_html_to_text(raw)
        title = u.replace(BASE, "").lstrip("/")
        parts.append(f"# {title}\n\n_Source: {md_url}_\n\n{cleaned}")

    OUT.write_text("\n\n---\n\n".join(parts), encoding="utf-8")
    print(f"Wrote {OUT.resolve()} with {len(urls)} sections.")

if __name__ == "__main__":
    main()
