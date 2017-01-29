README_bootctl.md
=================
You cannot create symlinks from these bootctl conf files
to the EFI System Partition (ESP) because the ESP uses FAT32
vfat filesystem by default. This fs does not support ACL's or
symlinks.

But you could probably use Ansible or Puppet to copy these
files to /boot/loader/ and /boot/loader/entries/
