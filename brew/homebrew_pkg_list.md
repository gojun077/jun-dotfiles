List of packages installed by brew
=====================================

# Summary

- Created on: Nov 24 2022
- Created by: gojun077@gmail.com
- Last Updated: Nov 24 2022


You can see a list of all packages installed with homebrew via `brew list`:

```sh
[peter.koh@peter brew]$ brew list
==> Formulae
ansible                 imlib2                  libusb                  pcre2
aom                     influxdb                libvidstab              perl
apr                     influxdb-cli            libvmaf                 pixman
apr-util                ipmitool                libvorbis               pkg-config
autoconf                jasper                  libvpx                  podman
bash                    jbig2dec                libx11                  portaudio
bdw-gc                  jemalloc                libxau                  pyenv
berkeley-db             jpeg                    libxcb                  python@3.10
boost                   jpeg-turbo              libxcursor              python@3.9
brotli                  jpeg-xl                 libxdmcp                pyyaml
c-ares                  jq                      libxext                 qemu
ca-certificates         krb5                    libxfixes               rav1e
cairo                   kube-linter             libxi                   readline
capstone                lame                    libxinerama             redis
ccache                  leptonica               libxrandr               rtmpdump
cjson                   lftp                    libxrender              rubberband
cmake                   libarchive              libxv                   screen
cmocka                  libass                  libyaml                 sdl2
cmus                    libb2                   little-cms2             shared-mime-info
coreutils               libbluray               lua                     shellcheck
curl                    libcaca                 lua@5.3                 sipcalc
dav1d                   libcbor                 lz4                     six
docbook                 libcue                  lzo                     snappy
docbook-xsl             libde265                m4                      speex
faad2                   libev                   mad                     sqlite
ffmpeg                  libevent                markdown                srt
flac                    libffi                  mbedtls                 subversion
fontconfig              libfido2                mono                    taglib
freerdp                 libheif                 mp4v2                   tcl-tk
freetype                libidn                  mpdecimal               telnet
frei0r                  libidn2                 mpg123                  tesseract
fribidi                 liblinear               mplayer                 tfenv
gdbm                    liblqr                  mr                      tflint
gettext                 libmicrohttpd           musikcube               the_silver_searcher
ghostscript             libnghttp2              mysql-client            theora
giflib                  libogg                  ncdu                    tree
git                     libomp                  ncurses                 tty-clock
git-filter-repo         libopenmpt              nettle                  unbound
glib                    libpng                  nghttp2                 utf8proc
gmp                     libpq                   nmap                    vde
gnu-getopt              libpthread-stubs        nuget                   webp
gnutls                  libraw                  oniguruma               wget
gobject-introspection   librist                 opencore-amr            x264
graphite2               libsamplerate           openexr                 x265
grep                    libslirp                openjdk                 xmlto
groovy                  libsndfile              openjpeg                xorgproto
guile                   libsodium               openldap                xvid
hadolint                libsoxr                 openssl@1.1             xz
harfbuzz                libssh                  openssl@3               youtube-dl
helm                    libssh2                 opus                    zeromq
hiredis                 libtasn1                opusfile                zimg
icu4c                   libtiff                 p11-kit                 zlib
imagemagick             libtool                 p7zip                   zstd
imath                   libunistring            pcre

==> Casks
amethyst                        font-monofur-nerd-font          quodlibet
audacity                        font-nanum-gothic               vlc
chromedriver                    font-nanum-gothic-coding        xquartz
emacs                           macfuse
emacsclient                     mactex
```

`brew leaves` shows a list of all top-level packages (i.e. non-dependencies):

```sh
[peter.koh@peter brew]$ brew leaves
ansible
bash
ccache
clangen/musikcube/musikcube
cmake
cmus
coreutils
freerdp
git
git-filter-repo
grep
groovy
hadolint
helm
imagemagick
influxdb
influxdb-cli
ipmitool
jq
kube-linter
lftp
libpq
lua
markdown
mplayer
mr
mysql-client
ncdu
nghttp2
nmap
nuget
p7zip
perl
podman
pyenv
redis
screen
shellcheck
sipcalc
subversion
telnet
tfenv
tflint
the_silver_searcher
tree
tty-clock
wget
youtube-dl
```

## References

- https://apple.stackexchange.com/questions/101090/list-of-all-packages-installed-using-homebrew
