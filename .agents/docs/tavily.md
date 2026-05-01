# tvly (Tavily CLI v0.1+) — Public web search, extract, crawl, deep research

LLM-optimized public web access. For internal wikis (Notion, Confluence,
private.docs.*), use the respective Notion or web-to-markdown tooling.

```bash
# Search
tvly search "<query>" --json

# Extract specific URL(s) — up to 20 in one call
tvly extract "<url>" --json

# Crawl a docs site (for agentic use, ALWAYS pass --instructions
# + --chunks-per-source to avoid context explosion)
tvly crawl "https://docs.example.com" \
  --instructions "API auth" --chunks-per-source 3 --json

# Deep AI research with citations (30-120s)
tvly research "<topic>" --model pro --stream
```

## Install & auth

Verify with `tvly --status` (should report "Authenticated via API key" or OAuth).
To install:

```bash
curl -fsSL https://cli.tavily.com/install.sh | bash
# or: uv tool install tavily-cli  /  pip install tavily-cli

# auth (any one):
tvly login --api-key tvly-YOUR_KEY
tvly login                          # browser OAuth
export TAVILY_API_KEY=tvly-YOUR_KEY
```

## Pick a command

| Need                                  | Command        |
| ------------------------------------- | -------------- |
| Find pages on a topic                 | `tvly search`  |
| Get a specific URL's content          | `tvly extract` |
| Bulk-extract a docs section           | `tvly crawl`   |
| Multi-source synthesis with citations | `tvly research`|

## Pick a tool (tvly vs alternatives)

| Surface                          | Tool              |
| -------------------------------- | ----------------- |
| Private GitHub repos             | `gh api`          |
| **Public web (everything else)** | `tvly`            |

Reach for `tvly` for: public framework/SDK docs (LangChain, vendor APIs, etc.),
latest model releases, competitive intel, and public references not mirrored
in private repos.

## Tips

- **Always quote URLs** — shell treats `?` and `&` as special.
- **Always use `--json`** for agentic workflows (every subcommand supports it).
- **Read from stdin with `-`** — `echo "query" | tvly search -`
- **JS-heavy / SPA pages**: `extract` and `crawl` accept `--extract-depth advanced`.
- **Crawl in agent loops**: pass `--instructions` + `--chunks-per-source 1-5` to
  return only relevant chunks rather than full pages.
- **Save data, not code**: persist `tvly` JSON to `/tmp/*.json` between turns;
  write filtering logic as heredocs, not one-shot `/tmp/*.py` files.
- **Exit codes**: 0=success, 2=bad input, 3=auth error, 4=API error.
