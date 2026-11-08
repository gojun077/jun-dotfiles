# Beads schema upgrades: remote-first adoption

`beads-schema-adopt` is a Python 3 standard-library script for **existing, running server-mode databases**, including databases on a shared Dolt server. It adopts schema history already published by a designated migrator; it does not independently execute migrations. Tested with Beads 1.3.4 and Dolt 2.7.2 on Linux. No installation or Python packages are required.

## Choose the role first

- **Follower (this script):** the remote already contains the required schema. Fetch and fast-forward to its history. Never set `BD_ALLOW_REMOTE_MIGRATE=1` or run `bd migrate schema --force` on each machine: independently generated schema histories may be unmergeable.
- **Designated migrator:** only when the remote still needs upgrading, coordinate one workspace and pause writers to that database across all machines/checkouts. Back up the database, synchronize its existing history, review the installed `bd migrate schema --help`, and deliberately run `bd migrate schema --force` there. Validate it and, with publication authorization, publish using `BEADS_DOLT_CLI_DIR=/absolute/database/path bd dolt push`. Only then update followers. This script does not automate that separately authorized role.
- **Divergent history or dirty data:** stop and reconcile deliberately. The script never hard-resets, deletes/re-clones a database, or guesses which issue edits to discard. A local history that already contains the remote is accepted as a no-op if Beads validation succeeds.

Use compatible Beads/Dolt versions on every machine; older clients may refuse a newer schema. A future release may change the CLI or SQL contract. Failures are stops for inspection, not permission to add `--force`.

## Preconditions

1. Pause agents and other writers **to the target database**, including linked worktrees. The script checks state again before merging, but does not provide a distributed maintenance lock or an atomic dump with concurrent ignored-table writes. Other databases can remain online.
2. Inspect `bd context` and `bd dolt show --json` in the workspace. Find the exact database directory on the same machine as its server, not the shared server's parent directory. Resolve symlinked/worktree `.beads` paths normally through Beads.
3. Confirm the intended remote and branch with `dolt remote -v` and `dolt branch -a` from that database directory. Arrange SSH, tailnet, or DoltHub access before running the script. It uses the existing named remote, not a hardcoded URL, and does not change authentication or networking.
4. The script verifies that Beads' explicit SQL endpoint and Dolt's directory-based automatic connection resolve to the same database, active branch, and HEAD. A workstation pointing at a remote SQL server without its local database directory is outside its scope.
5. If SQL authentication is needed, supply `BEADS_DOLT_PASSWORD` and/or `DOLT_CLI_PASSWORD` via your secret manager or environment, not arguments or source files. Dolt does not read Beads' credentials file; if Beads reads that file, supply the same password through `DOLT_CLI_PASSWORD`. TLS follows `bd dolt show`. This password/TLS path has not been tested against an authenticated or TLS-enabled server.

## Run

```sh
# Inspect/fetch only: no working-branch change, backup, migration, or push.
~/dotfiles/bin/beads-schema-adopt /path/to/workspace \
  --dolt-dir "$HOME/.beads/shared-server/dolt/DATABASE"

# Review the database, local/remote schema versions, and commit hashes above.
~/dotfiles/bin/beads-schema-adopt /path/to/workspace \
  --dolt-dir "$HOME/.beads/shared-server/dolt/DATABASE" --apply
```

Optional `--remote NAME` and `--branch NAME` default to `origin` and `main`. The script refuses to switch the active branch. Inspection fetches remote-tracking refs; it is not an entirely mutation-free dry-run. Exit 0 means either a viable inspection plan, validated adoption, or validated already-incorporated history; inspect the final message to distinguish them. Nonzero means stop and inspect; adoption may already have succeeded if subsequent validation failed.

## Deterministic sequence

1. Resolve the Beads server endpoint and check directory identity, active branch, clean tracked working set, and absence of an active merge. Refuse migration-override environment variables.
2. Run `dolt fetch REMOTE` from the verified database directory. This CLI route supports git+SSH and DoltHub remotes; only git+SSH was exercised in the validation run.
3. Read local and remote `schema_migrations` versions, resolve the fetched remote branch to an immutable hash, and compute `DOLT_MERGE_BASE('HEAD', HASH)`. Reject divergent histories and downgrades. Do not infer ancestry from row counts or schema version alone.
4. With `--apply`, create a private directory under `${XDG_STATE_HOME:-$HOME/.local/state}/beads-schema/`. Run `dolt dump --no-create-db --file-name …/before.sql` and save `state.json`. The dump includes ignored clone-local tables; a committed-history branch alone does not back those up. Recheck the working state and retain a uniquely named `schema-backup-…` branch at the original HEAD.
5. Connect to the selected database on the live SQL server and execute `CALL DOLT_MERGE('--ff-only', PINNED_HASH)`. No independent DDL migrations or merge commit are generated. Dolt 2.7.2 standalone `dolt merge` refuses a running local server; SQL is required. `dolt dump` conversely rejects explicit global connection flags, so it uses the already-verified directory route.
6. Verify the new HEAD equals the pinned target, the schema version matches, and the tracked working set is clean. Run `bd --readonly ready --json` and `bd stats`. Run `bd doctor` separately and review warnings without indiscriminate `--fix`.

The script never restarts/stops the server, switches tailnets, modifies sibling databases, invokes a hard reset, pushes anything, or runs `bd migrate schema --force`. `bd` may still maintain clone-local metadata when opening a database; a clean `dolt_status` excludes ignored tables.

## Recovery and limits

Keep the printed recovery directory and branch until normal work has been verified. Dumps contain private issue data and must not go into this public dotfiles repository. Do not restore into the shared root or automatically reset the live database. First inspect its HEAD, `dolt_status`, and whether the pinned target was adopted. Restore the SQL dump into a separately provisioned recovery database to inspect clone-local data; recover committed history from the recovery branch. Replacing the live database requires separate review and downtime coordination for that database.

A fresh remote clone lacks ignored local tables such as `leases`, `wisps`, and `local_metadata`. This is not the same case as updating an existing populated database: the script's read-only validation will stop if those tables are missing. Use the installed Beads fresh-clone/bootstrap procedure deliberately; do not interpret a missing-local-table error as permission to generate a new tracked schema migration on a follower.

## Validation performed

The script was exercised against a real v69 database reconstructed on a disposable Dolt SQL server, with its clone-local state restored from a pre-adoption dump. Inspection left HEAD unchanged; `--apply` backed up and adopted v74 with all 244 issues; a second apply was a no-op. Dirty working sets, a divergent commit, and a directory pointing at a different database were rejected. The divergent HEAD stayed unchanged. The live shared-server database also passed the already-incorporated check. No remote publication was performed. Authenticated/TLS connections, DoltHub transport, and designated-migrator execution were not tested.
