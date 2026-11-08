# Age of Wonders III (Steam, native Linux build) on Asahi Linux

Steam app 226840, native 32-bit Linux build, run by Steam through FEX + muvm
inside the Steam Linux Runtime (scout-on-soldier). Tested on a MacBook Air M2
with Fedora Asahi Remix 45, fex-emu 2604 and the Fedora 45 FEX rootfs.

Keep Steam on the native build: leave Properties → Compatibility unforced
(no Proton).

## Problem

The launcher works and presses Play, but the game exits immediately:

```
./libiggy_linux.so: undefined symbol: pthread_yield
```

The FEX rootfs ships i386 glibc 2.44, where `pthread_yield` is only a hidden
compat symbol (`pthread_yield@GLIBC_2.2`). The game's bundled
`libiggy_linux.so` references it unversioned, which the dynamic loader never
binds to a hidden symbol. pressure-vessel uses this newer host glibc rather
than the runtime's, so the game fails inside Steam too.

## Fix

- `pthread_yield_shim.c`: source for `pthread_yield_shim.so`, an i386 shared
  library exporting `pthread_yield`, which calls `sched_yield`. The built
  `.so` is not committed (`*.so` is gitignored); build it with `make`.
- `AoW3Direct.sh`: starts `./AoW3` directly, skipping AoW3Launcher. It reuses
  the library symlink fixes and env exports from the game's own
  `AoW3Launcher.sh` (everything but its final `exec`), preloads the shim, and
  runs `./AoW3 -profile GUEST -offline`, the arguments the launcher passes.
  Remove `-offline` if you want online features.

## Build and install

Requires `clang` and `lld` (no i386 sysroot needed):

```sh
make            # builds pthread_yield_shim.so
make install    # copies the shim and AoW3Direct.sh into the game folder
```

`GAME_DIR` defaults to `~/.local/share/Steam/steamapps/common/AoW3`; override
it with `make install GAME_DIR=/path/to/AoW3`. Steam's "Verify integrity of
game files" leaves both files in place.

## Steam launch option

Paste into Properties → General → Launch Options:

```
bash -c 'exec "${@/AoW3Launcher.sh/AoW3Direct.sh}"' -- %command%
```

Steam's native launch command ends with `AoW3Launcher.run ./AoW3Launcher.sh`,
and `AoW3Launcher.run` only calls `system(argv[1])`. The bash substitution
swaps that script for `AoW3Direct.sh` and leaves the rest of the Steam Linux
Runtime chain untouched.

To keep the launcher and only add the shim, use this instead (untested):

```
LD_PRELOAD="$HOME/.local/share/Steam/steamapps/common/AoW3/pthread_yield_shim.so${LD_PRELOAD:+:$LD_PRELOAD}" %command%
```

`wrong ELF class: ELFCLASS32` warnings in the logs come from 64-bit helper
processes skipping the 32-bit preload and are harmless.
