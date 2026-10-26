-- original path to file: $HOME/.wezterm.lua
-- PJ's Wezterm Config for MacOS
-- Created on: May 12 2024
-- Created by: gopeterjun@naver.com
-- Last Updated: Wed 12 Aug 2026

-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices
config.color_scheme = 'duskfox'
config.font = wezterm.font 'Monofur Nerd Font Mono'
config.font_size = 15.5
config.enable_tab_bar = false

config.colors = {
    cursor_bg = '#52ad70',
    cursor_fg = 'maroon'
}

config.ssh_domains = {
  {
    -- This 'name' is what you will use in the connect command.
    name = 'ocloud-arm',
    -- The actual IP address or domain of your remote server.
    remote_address = 'pj-arm64-ampere.finch-blues.ts.net',
    -- Your username on the remote server.
    username = 'ubuntu',
    -- Explicitly tell WezTerm to use its own multiplexing.
    -- This is the default behavior, so you can omit it if you prefer.
    multiplexing = 'WezTerm',
  }
}

-- and finally, return the configuration to wezterm
return config
