MNEMOSYNE CONF FILES
====================+

# Summary
- Last Updated: 2019.06.07
- Updated by: gojun077@gmail.com

> The original path for these files is `~/.config/mnemosyne/`.
> `config.py` contains settings such as the latex preamble which
> specifies latex fonts to use in mnemosyne cards

> `machine.id` is used to identify the machine for card syncing with
> other devices or machines

> `config.db` stores config values of some kind, but I haven't
> accessed this DB yet. It is probably in sqlite3 format like
> `default.db`

> Symlinks are created from `~/dotfiles/mnemosyne-conf/[FILE]` to
> `~/.config/mnemosyne/[FILE]` with the exception of `machine.id`
> which is unique per machine.

## Share mnemosyne cards across multiple machines using Dropbox

> The default path for mnemosyne card DB is
> `$HOME/.local/share/mnemosyne`. I store all my Mnemosyne cards
> on Dropbox with the PATH `$HOME/Dropbox/mnemosyne`. To make sure
> a local instance of Mnemosyne uses cards from Dropbox, first remove
> the local mnemosyne folder, then make a symlink from mnemosyne on
> Dropbox to `~/.local/share`

```sh
rm -rf ~/.local/share/mnemosyne
ln -s $HOME/Dropbox/mnemosyne/ $HOME/.local/share/
```




## References

https://eatpeppershothot.blogspot.com/2015/12/sharing-mnemosyne-cards-across-multiple.html
