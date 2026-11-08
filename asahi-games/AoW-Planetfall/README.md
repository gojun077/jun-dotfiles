# Age of Wonders: Planetfall (Steam, Windows build) on Asahi Linux

Steam app 718850, Windows-only 64-bit build, run by Steam through Proton
Experimental (11.0) on the Steam Linux Runtime, under FEX + muvm. Tested on a
MacBook Air M2 with Fedora Asahi Remix 45, fex-emu 2604, muvm 0.7.0 and the
Honeykrisp Vulkan driver (Mesa 26.2).

Set Properties → Compatibility → "Force the use of a specific Steam Play
compatibility tool" to Proton Experimental.

No files need to be installed; the fix is the launch option alone.

## Steam launch option

Paste into Properties → General → Launch Options:

```
DXVK_CONFIG="d3d11.disableMsaa = True" bash -c 'exec "${@/dowser.exe/AowPF.exe}"' -- %command%
```

The two parts fix two separate problems, described below.

## Problem 1: the Paradox launcher dies

Steam's launch target is `dowser.exe`, which installs and starts the Paradox
Launcher (v2019.10.3). Under Proton on Asahi the launcher starts and then
exits about 40 seconds later without ever running the game.

The bash substitution replaces `dowser.exe` with `AowPF.exe` in Steam's
command, so Steam starts the game directly. The Steam Linux Runtime, Proton,
and the Steam overlay stay in the chain (`"/path/to/AowPF.exe" %command%`
would drop Proton). DLC in the game folder still loads; there is no launcher
to manage mods.

## Problem 2: crash on the loading screen (8x MSAA)

The game reaches the loading screen and then crashes about 40 seconds in with
an access violation at `AowPF.exe+0x86de6e`. With `PROTON_LOG=1`, DXVK logs
the cause just before it:

```
err:   D3D11: Cannot create texture:
err:     Format:  44
err:     Extent:  64x64x1
err:     Samples: 8
```

The game creates two 8x multisampled depth textures (64x64 and window-sized)
at startup. The M2 GPU supports at most 4x MSAA (`vulkaninfo` reports
`framebufferDepthSampleCounts` 1, 2, 4), so the creation fails and the game
dereferences the null texture.

The 8x request does not depend on the in-game settings: it still happens with
`GraphicsOptions.AntiAliasMode=0`, `EdgeBlurEnabled=false` and
`AmbientOcclusionQuality=0` in `Settings.cfg`. DXVK has no option to cap the
sample count at 4x, so `DXVK_CONFIG="d3d11.disableMsaa = True"` forces all
textures to 1 sample. In-game MSAA has no effect as a result; Edge Blur (a
post-process smoother) and a Render Scale above 1.0 still work.

## Known issue: crash on exit

Quitting the game shows the "Age of Wonders: Planetfall has crashed" dialog.
A background thread in `pops_api.dll` (Paradox online services) reads freed
memory during shutdown. Settings and cloud saves are written before the
crash, so it is harmless; close the dialog. Steam keeps the game marked as
running until the dialog is closed.

## Logs and crash dumps

- Proton log: add `PROTON_LOG=1` in front of the launch option; the log goes
  to `~/steam-718850.log`.
- Game crash dumps:
  `steamapps/compatdata/718850/pfx/drive_c/users/steamuser/AppData/Local/Temp/DMP*.dmp`
- Game settings:
  `steamapps/compatdata/718850/pfx/drive_c/users/steamuser/Documents/Paradox Interactive/Age of Wonders Planetfall/Config/Settings.cfg`

The `pressure-vessel-wrap` "Internal error" lines about Vulkan layers,
`pv-locale-gen` messages, and `gameoverlayrenderer.so ... cannot be
preloaded` errors in Steam's terminal output are harmless under muvm.
