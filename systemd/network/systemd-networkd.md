systemd-networkd Notes
==========================


`systemd-networkd` is a systemd service that defines and manages network
resources using static files read from /etc/systemd/network. It is included
as part of the `systemd` package but `systemd-network.service` is not enabled
by default.

You can verify that it belongs to the `systemd` package with the following
commands:

```sh
# On Archlinux:
$ sudo pacman -Fy systemd-networkd
core/systemd 249.3-1 [installed]
    usr/lib/systemd/systemd-networkd

# On Ubuntu:
$ dpkg -S systemd-networkd
systemd: /lib/systemd/systemd-networkd-wait-online
systemd: /lib/systemd/systemd-networkd
systemd: /lib/systemd/system/systemd-networkd.socket
systemd: /usr/share/man/man8/systemd-networkd-wait-online.8.gz
systemd: /var/lib/polkit-1/localauthority/10-vendor.d/systemd-networkd.pkla
systemd: /usr/share/man/man8/systemd-networkd-wait-online.service.8.gz
systemd: /usr/share/man/man8/systemd-networkd.service.8.gz
systemd: /lib/systemd/system/systemd-networkd-wait-online.service
systemd: /lib/systemd/system/systemd-networkd.service
systemd: /usr/share/man/man8/systemd-networkd.8.gz
systemd: /usr/share/polkit-1/rules.d/systemd-networkd.rules
```

`systemd-networkd` supersedes distro-specific network file syntax schemes
like `netctl` on Archlinux, `netplan` on Ubuntu, `/etc/network/interfaces` on
legacy Ubuntu, `/etc/sysconfig/network-scripts/` on Fedora/RHEL, and so on. 

