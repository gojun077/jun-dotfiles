-- init.lua
--
-- original path to file: ~/.config/rex/init.lua
--
-- Created on: Tue 27 Oct 2026
-- Created by: gopeterjun@naver.com
-- Last Updated: Wed 28 Oct 2026
--
-- Rex multiplexer configuration for macOS and Linux.
-- Validate with: rex config check
-- Apply with:    rex config reload

-- ============================================================================
-- macOS Client Typography, Cursor & Ghostty Dark Theme (Rex.app UserDefaults)
-- ============================================================================
-- Rex splits configuration between the server (init.lua: keybindings, actions,
-- modes, hosts) and the macOS GUI client (com.superlogical.rex UserDefaults:
-- font family, font size, cursor, themes, appearance).
-- Mirror ~/dotfiles/ghostty.config + Ghostty's default dark palette:

local function hex_to_srgb_json(hex)
  local h = hex:gsub("^#", "")
  local r = tonumber(h:sub(1, 2), 16) / 255.0
  local g = tonumber(h:sub(3, 4), 16) / 255.0
  local b = tonumber(h:sub(5, 6), 16) / 255.0
  return string.format(
    '{"colorSpace":"srgb","red":%.8f,"green":%.8f,"blue":%.8f,"alpha":1}',
    r, g, b
  )
end

local function str_to_hex(s)
  return (s:gsub(".", function(c)
    return string.format("%02x", string.byte(c))
  end))
end

-- Colors matching ~/dotfiles/ghostty.config + `ghostty +show-config --default`
local ghostty_dark = {
  id = "custom-ghostty-dark",
  name = "Ghostty Default Dark",
  background          = "#282c34",
  foreground          = "#ffffff",
  cursor              = "#ff2d95", -- cursor-color = #ff2d95 in ghostty.config
  cursorText          = "#000000", -- cursor-text  = #000000 in ghostty.config
  selectionBackground = "#3e4451",
  selectionForeground = "#ffffff",
  ansi = {
    "#1d1f21", -- 0: black
    "#cc6666", -- 1: red
    "#b5bd68", -- 2: green
    "#f0c674", -- 3: yellow
    "#81a2be", -- 4: blue
    "#b294bb", -- 5: magenta
    "#8abeb7", -- 6: cyan
    "#c5c8c6", -- 7: white
    "#666666", -- 8: bright black
    "#d54e53", -- 9: bright red
    "#b9ca4a", -- 10: bright green
    "#e7c547", -- 11: bright yellow
    "#7aa6da", -- 12: bright blue
    "#c397d8", -- 13: bright magenta
    "#70c0b1", -- 14: bright cyan
    "#eaeaea", -- 15: bright white
  },
}

local ansi_json = {}
for i, c in ipairs(ghostty_dark.ansi) do
  ansi_json[i] = hex_to_srgb_json(c)
end

local custom_themes_json = string.format(
  '[{"id":%q,"name":%q,"appearance":"dark","createdAt":783327600,'
    .. '"source":{"ghosttyMigration":{}},'
    .. '"palette":{"background":%s,"foreground":%s,"cursor":%s,"cursorText":%s,'
    .. '"selectionBackground":%s,"selectionForeground":%s,'
    .. '"ansi":[%s],"paletteGenerate":false,"paletteHarmonious":false}}]',
  ghostty_dark.id,
  ghostty_dark.name,
  hex_to_srgb_json(ghostty_dark.background),
  hex_to_srgb_json(ghostty_dark.foreground),
  hex_to_srgb_json(ghostty_dark.cursor),
  hex_to_srgb_json(ghostty_dark.cursorText),
  hex_to_srgb_json(ghostty_dark.selectionBackground),
  hex_to_srgb_json(ghostty_dark.selectionForeground),
  table.concat(ansi_json, ",")
)

-- Typography (Monofur Nerd Font Mono, size 16)
rex.exec({
  "/usr/bin/defaults", "write", "com.superlogical.rex",
  "Workspace.terminalFontName", "-string", "Monofur Nerd Font Mono",
})
rex.exec({
  "/usr/bin/defaults", "write", "com.superlogical.rex",
  "Workspace.terminalFontPointSize", "-float", "16",
})

-- Cursor & Option key (matching ghostty.config: block, no blink, left option as Alt)
rex.exec({
  "/usr/bin/defaults", "write", "com.superlogical.rex",
  "Workspace.terminalCursorStyle", "-string", "block",
})
rex.exec({
  "/usr/bin/defaults", "write", "com.superlogical.rex",
  "Workspace.terminalCursorBlinkIntervalMilliseconds", "-int", "0",
})
rex.exec({
  "/usr/bin/defaults", "write", "com.superlogical.rex",
  "Workspace.terminalOptionKeyBehavior", "-string", "leftActsAsAlt",
})

-- Dark appearance & Ghostty custom theme palette
rex.exec({
  "/usr/bin/defaults", "write", "com.superlogical.rex",
  "Workspace.customThemes", "-data", str_to_hex(custom_themes_json),
})
rex.exec({
  "/usr/bin/defaults", "write", "com.superlogical.rex",
  "Workspace.appearance", "-string", "dark",
})
rex.exec({
  "/usr/bin/defaults", "write", "com.superlogical.rex",
  "Workspace.darkThemeID", "-string", ghostty_dark.id,
})
rex.exec({
  "/usr/bin/defaults", "write", "com.superlogical.rex",
  "Workspace.darkUsesExtendedThemePalette", "-bool", "false",
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

-- ============================================================================
-- Remove Rex defaults that conflict with global macOS hotkeys
-- (AeroSpace ~/.aerospace.toml, Maccy clipboard history)
-- ============================================================================
-- AeroSpace and Maccy register global macOS hotkeys, so they win these
-- chords before Rex.app sees them; the Rex defaults below are dead while
-- those apps run.
-- Unbinding keeps the keymap honest (no bindings that silently resurrect
-- if AeroSpace is off). Each action stays reachable via the Ctrl+B prefix
-- above, except where noted. Spellings must match the defaults exactly
-- (e.g. cmd+[Digit1] is the physical digit key, distinct from cmd+1).

-- Cmd+0..9: AeroSpace `workspace N` vs Rex `client.tab.goto` for 1..9
-- (Cmd+9 is Rex "last tab", index -1; Cmd+0 is Rex `client.font.reset`).
-- Ctrl+B 1..9 / 0 already cover tab jumps; reset font instead with:
--   rex do client.font.reset
for i = 0, 9 do
  rex.unbind("cmd+[Digit" .. i .. "]")
end

-- Cmd+Left/Right: AeroSpace `workspace prev/next` vs Rex sending Ctrl+A/E
-- (just type Ctrl+A / Ctrl+E directly in the terminal).
rex.unbind("cmd+left")
rex.unbind("cmd+right")

-- Cmd+Alt+arrows: AeroSpace `focus-monitor prev/next` (Left/Right) vs Rex
-- pane focus. Up/Down have no AeroSpace counterpart, but are dropped too
-- for consistency; Ctrl+B arrows already cover pane focus in all directions.
rex.unbind("alt+cmd+left")
rex.unbind("alt+cmd+right")
rex.unbind("alt+cmd+up")
rex.unbind("alt+cmd+down")

-- Cmd+/: AeroSpace `layout tiles horizontal vertical` vs Rex sidebar toggle.
rex.unbind("cmd+/")

-- Shift+Cmd+P: Maccy clipboard history popup (global hotkey) vs Rex command
-- palette (`client.palette.toggle`). Open the palette instead with:
--   rex do client.palette.toggle
rex.unbind("shift+cmd+p")

-- Deliberately kept: Cmd+, (Rex Settings) -- no Ctrl+B replacement bound.

-- Session switcher
rex.bind("ctrl+b>s", "session.switch")

-- Press Ctrl+B twice to send a literal Ctrl+B to the focused terminal
rex.bind("ctrl+b>ctrl+b", "pane.send_key", { key = "ctrl+b" })
