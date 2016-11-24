MNEMOSYNE CONF FILES
====================
The original path for these files is _~/.config/mnemosyne/_

- `config.py` contains settings such as the latex preamble
  which specifies latex fonts to use in mnemosyne cards

- `machine.id` is used to identify the machine for card
  syncing with other devices or machines

- `config.db` stores config values of some kind, but I haven't
  accessed this DB yet. It is probably in sqlite3 format like
  `default.db`

Symlinks will be created from ~/dotfiles/mnemosyne-conf/[FILE]
to ~/.config/mnemosyne/[FILE] with the exception of `machine.id`
which is unique per machine.
