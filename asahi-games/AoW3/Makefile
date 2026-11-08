# Cross-builds the i386 pthread_yield shim on aarch64 with clang + lld.
# -nostdlib: no i386 sysroot is needed; sched_yield is resolved at runtime
# from the game's already-loaded libc.

GAME_DIR ?= $(HOME)/.local/share/Steam/steamapps/common/AoW3

pthread_yield_shim.so: pthread_yield_shim.c
	clang --target=i686-linux-gnu -m32 -O2 -fPIC -shared -nostdlib -fuse-ld=lld \
		-Wl,-soname,pthread_yield_shim.so -o $@ $<

install: pthread_yield_shim.so AoW3Direct.sh
	install -m 755 pthread_yield_shim.so AoW3Direct.sh $(GAME_DIR)/

.PHONY: install
