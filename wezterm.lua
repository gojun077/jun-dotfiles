-- original path to file: $HOME/.wezterm.lua
-- PJ's Wezterm Config
-- Created on: May 8 2024
-- Created by: gopeterjun@naver.com
-- Last Updated: May 26 2024

-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices
config.color_scheme = 'duskfox'
-- the following font name is only valid on Linux
--config.font = wezterm.font 'MonofurNerdFontMono'
config.font = wezterm.font 'Monofur Nerd Font Mono'
config.font_size = 16.0
config.enable_tab_bar = false

-- and finally, return the configuration to wezterm
return config
