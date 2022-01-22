UFW Port-Forwarding README
==============================

The settings in these files are for enabling port-forwarding in
the UFW firewall from a wired internal network out through a
wireless interface on the host running UFW. This is to enable
hosts with only a wired internal connection to send and receive
packets with the Internet.

Note that in addition to the 3 files

- `/etc/ufw/before.rules`
- `/etc/ufw/sysctl.conf`
- `/etc/default/ufw`

for UFW, you also need to enable `net.ipv4.ip_forward=1` in
`sysctl` by creating a file in `/usr/lib/sysctl.d/`, for example
`98-ipv4-forward.conf` containing the line

`net.ipv4.ip_forward=1`
