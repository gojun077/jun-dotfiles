# Game fixes for Asahi Linux

Shims, launch scripts, and patches that make Steam, GOG, and other x86 games
run on Asahi Linux (Apple Silicon) under FEX + muvm. One sub-folder per game,
each with its own README covering the problem, the build (if any), and the
exact Steam launch option to use.

| Game | Store | Folder | Fix |
| --- | --- | --- | --- |
| Age of Wonders III | Steam (native Linux) | [`AoW3/`](AoW3/) | `pthread_yield` shim + direct launch script |
| Age of Wonders: Planetfall | Steam (Windows, Proton) | [`AoW-Planetfall/`](AoW-Planetfall/) | Launch option: skip Paradox launcher + disable MSAA in DXVK |
