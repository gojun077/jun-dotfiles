-- init.lua
--
-- original path to file: ~/.config/rex/init.lua
--
-- Created on: Tue 27 Oct 2026
-- Created by: gopeterjun@naver.com
-- Last Updated: Tue 27 Oct 2026
--
-- Rex multiplexer configuration for macOS and Linux.
-- Validate with: rex config check
-- Apply with:    rex config reload

-- ============================================================================
-- macOS Client Typography (Rex.app UserDefaults)
-- ============================================================================
-- Rex splits configuration between the server (init.lua: keybindings, actions,
-- modes, hosts) and the macOS GUI client (com.superlogical.rex UserDefaults:
-- font family, font size, cursor, themes).
-- Ensure Monofur Nerd Font Mono (size 16, matching Ghostty) is set on macOS:
rex.exec({
  "/usr/bin/defaults", "write", "com.superlogical.rex",
  "Workspace.terminalFontName", "-string", "Monofur Nerd Font Mono",
})
rex.exec({
  "/usr/bin/defaults", "write", "com.superlogical.rex",
  "Workspace.terminalFontPointSize", "-float", "16",
})

-- ============================================================================
-- Tmux Muscle-Memory Keybindings (Prefix: Ctrl+B)
-- ============================================================================

-- Navigate to next / previous tab (window) in the current session
rex.bind("ctrl+b>n", "client.tab.next")
rex.bind("ctrl+b>p", "client.tab.previous")

-- Jump directly to tab by number (Rex tabs are 1-indexed; 0 jumps to tab 10)
for i = 1, 9 do
  rex.bind("ctrl+b>" .. i, "client.tab.goto", { index = i })
end
rex.bind("ctrl+b>0", "client.tab.goto", { index = 10 })

-- Create and rename tabs
rex.bind("ctrl+b>c", "client.tab.new")
rex.bind("ctrl+b>,", "client.tab.rename")

-- Split panes: Ctrl+B % (right) and Ctrl+B " (down) on US layout
rex.bind("ctrl+b>shift+5", "pane.split", { direction = "right" })
rex.bind("ctrl+b>shift+'", "pane.split", { direction = "down" })

-- Pane navigation, zoom, and close
rex.bind("ctrl+b>o", "pane.focus_next")
rex.bind("ctrl+b>left", "pane.focus", { direction = "left" })
rex.bind("ctrl+b>right", "pane.focus", { direction = "right" })
rex.bind("ctrl+b>up", "pane.focus", { direction = "up" })
rex.bind("ctrl+b>down", "pane.focus", { direction = "down" })
rex.bind("ctrl+b>z", "pane.zoom")
rex.bind("ctrl+b>x", "pane.close")

-- Session switcher
rex.bind("ctrl+b>s", "session.switch")

-- Press Ctrl+B twice to send a literal Ctrl+B to the focused terminal
rex.bind("ctrl+b>ctrl+b", "pane.send_key", { key = "ctrl+b" })
