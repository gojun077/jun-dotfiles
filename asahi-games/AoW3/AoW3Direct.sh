#!/bin/bash
# Start AoW3 directly, skipping AoW3Launcher.
# Reuses the symlink fixes and env exports from AoW3Launcher.sh (everything but its final exec),
# then preloads a shim that supplies pthread_yield, which glibc >= 2.34 hides from libiggy_linux.so.
cd "$(dirname "$0")" || exit 1
# shellcheck source=/dev/null
source <(grep -v '^exec ' ./AoW3Launcher.sh)
export LD_PRELOAD="$PWD/pthread_yield_shim.so${LD_PRELOAD:+:$LD_PRELOAD}"
# Same arguments AoW3Launcher passed when it pressed Play.
exec ./AoW3 -profile GUEST -offline
