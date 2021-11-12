lftp_README.md
=============================

This directory contains `lftprc` config files which are read from
`~/.lftprc`. The files in the this dir are intended to be symlinked
to `~/.lftprc` according to your needs.

To see a list of all config vars, execute `lftp` as follows:

`lftp ftp://user@some.ftp.org -e "set -a"`

Note: it is also possible to make command settings from the CLI
when invoking `lftp` instead of relying on `.lftprc`:

Launch `lftp` in debug mode and don't use TLS/SSL

```sh
lftp -d ftp://projectdrop@newftp.test.it.unity3d.com -e \
  "set ftp:ssl-allow false"
```

Launch `lftp` in debug mode and force use of TLS/SSL

```sh
lftp -d ftp://projectdrop@newftp.test.it.unity3d.com -e \
  "set ftp:ssl-force true"
```

