-- original path to file: $HOME/.wezterm.lua
-- PJ's Wezterm Config for MacOS on Macbook Air M2
-- Created on: Jun 9 2024
-- Created by: gopeterjun@naver.com
-- Last Updated: Jun 9 2024

-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices
config.color_scheme = 'duskfox'
config.font = wezterm.font 'Monofur Nerd Font Mono'
config.font_size = 17
config.enable_tab_bar = false

-- and finally, return the configuration to wezterm
return config
