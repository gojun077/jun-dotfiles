README_sysctld.md
=================
This directory contains files which should reside in
/etc/sysctl.d/
In Archlinux circa Jan 2017, `/etc/sysctl.conf` is deprecated.
As sysctl settings are now stored in separate conf files under
/etc/sysctl.d/ or `/usr/lib/sysctl.d/`. Systemd will check these
directories for kernel parameters residing in `*.conf` files.
