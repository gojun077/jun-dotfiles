systemd/environment.d
=========================

From https://www.freedesktop.org/software/systemd/man/environment.d.html

> `environment.d` — Definition of user service environment
>
> - `~/.config/environment.d/*.conf`
> - `/etc/environment.d/*.conf`
> - `/run/environment.d/*.conf`
> - `/usr/lib/environment.d/*.conf`
> - `/etc/environment`

> Configuration files in the environment.d/ directories contain lists of
> environment variable assignments for services started by the systemd user
> instance. systemd-environment-d-generator(8) parses them and updates the
> environment exported by the systemd user instance. See below for an
> discussion of which processes inherit those variables.

> It is recommended to use numerical prefixes for file names to simplify
> ordering.

> For backwards compatibility, a symlink to /etc/environment is installed,
> so this file is also parsed.

The files in this directory declare environment variables that need to
be available to systemd user services that are launched via
`systemctl start --user <my-user-service>`
