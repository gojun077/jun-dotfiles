---
name: tavily
description: "Public web search, content extraction, site crawling, and AI-powered deep research using the Tavily CLI (tvly). Use when asked to search the web, fetch page content from a URL, bulk-extract documentation, or synthesize multi-source research with citations."
---

# Tavily

Public web access via `tvly` CLI — search, extract, crawl, and deep research.
See `.agents/docs/tavily.md` for the full `tvly` reference.

## Prerequisites

- `tvly` CLI v0.1+ installed (`tvly --version` to verify)
- Authenticated via API key or OAuth (`tvly --status` to verify)

### Install & Auth

```bash
curl -fsSL https://cli.tavily.com/install.sh | bash
# or: uv tool install tavily-cli  /  pip install tavily-cli

# auth (any one):
tvly login --api-key tvly-YOUR_KEY
tvly login                          # browser OAuth
export TAVILY_API_KEY=tvly-YOUR_KEY
```

Verify with `tvly --status` (should report "Authenticated via API key" or OAuth).

## Agent Flags (ALWAYS use)

```
--json
```

## Capabilities

### 1. Search the web

```bash
tvly search "<query>" --json
```

Find pages on a topic. For agent loops, pipe through a Python sandbox to filter
results before they enter context:

```bash
echo "<query>" | tvly search - --json | python3 -c "
import json, sys
data = json.load(sys.stdin)
for r in data.get('results', [])[:5]:
    print(f\"{r['title']} \u2014 {r['url']}\n{r.get('content','')[:200]}\n\")
"
```

### 2. Extract content from specific URLs

```bash
tvly extract "<url>" --json
```

Extract clean, LLM-optimized content. Up to 20 URLs in one call:

```bash
tvly extract "https://example.com" --json
tvly extract "https://a.com" "https://b.com" --json  # up to 20
```

For JS-heavy / SPA pages, use advanced extraction:

```bash
tvly extract "<url>" --extract-depth advanced --json
```

### 3. Crawl a docs site

```bash
tvly crawl "https://docs.example.com" \
  --instructions "API auth" --chunks-per-source 3 --json
```

**Always** pass `--instructions` + `--chunks-per-source` (1-5) for agentic use.
Without these, full pages flood context.

### 4. Deep AI research with citations

```bash
tvly research "<topic>" --model pro --stream
```

Multi-source synthesis with citations. Takes 30-120s. Stream mode (`--stream`)
prints results incrementally.

## Routing — pick the right tool

| Source | Tool |
|--------|------|
| Public web — known URL | `tvly extract` |
| Public web — need to find pages first | `tvly search` |
| Public docs site — bulk extract | `tvly crawl` |
| Multi-source public research with citations | `tvly research` |
| Internal wiki / private docs | `web-to-markdown` |
| Notion | `ncli` |
| GitHub repos (public) | `tvly extract` or `gh api` |
| GitHub repos (private) | `gh api` |

## Tips

- **Always quote URLs** — shell treats `?` and `&` as special.
- **Always use `--json`** for agentic workflows.
- **Read from stdin with `-`** — `echo "query" | tvly search -`
- **Crawl in agent loops**: pass `--instructions` + `--chunks-per-source 1-5`.
- **Save data, not code**: persist `tvly` JSON to `/tmp/*.json` between turns;
  write filtering logic as heredocs.
- **Exit codes**: 0=success, 2=bad input, 3=auth error, 4=API error.
