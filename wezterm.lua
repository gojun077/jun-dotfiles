-- original path to file: $HOME/.wezterm.lua
-- PJ's Wezterm Config
-- Created on: May 8 2024
-- Created by: gopeterjun@naver.com
-- Last Updated: May 8 2024

-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices
config.color_scheme = 'duskfox'
config.font = wezterm.font 'MonofurNerdFontMono'
config.font_size = 14.0
config.enable_tab_bar = false

-- and finally, return the configuration to wezterm
return config
