# Elisp tooling for agentic workflows.
# Modeled after Go's `go doc` (docs lookup) and `gopls` (diagnostics).
#
# Docs/search hit the running Emacs server via emacsclient so they see your
# loaded init and packages. Lint runs `emacs --batch -Q` in isolation so it
# never touches the live session and is safe in CI.

.PHONY: help get_elisp_info validate_elisp search_elisp

help:
	@echo "Elisp agent tooling:"
	@echo "  make get_elisp_info SYMBOL=<name>   # docstring + signature + source file"
	@echo "  make validate_elisp FILE=<path.el>  # byte-compile lint (warnings = errors)"
	@echo "  make search_elisp QUERY=<regexp>    # apropos symbol search"

get_elisp_info:
	@bin/elisp-doc "$(SYMBOL)"

validate_elisp:
	@bin/elisp-lint "$(FILE)"

search_elisp:
	@bin/elisp-search "$(QUERY)"
