#!/bin/bash
# setup_wireguard.sh
#
# Last Updated: 2018.11.02
# Updated by: jun.go@actwo.com
#
# This script assumes that wireguard is already installed on a
# system. It will create the necessary directories, generate
# a wireguard public-private keypair, and generate a sample
# config file at /etc/wireguard/wg0.conf for a wireguard VPN
# server. This script must be executed as root user!

if [ "$USER" != "root" ]; then
  printf "%s\\n" "### This script must be executed as root user! ###"
  exit 1
fi

if ! [ -f /usr/bin/wg-quick ]; then
  printf "%s\\n" "### Wireguard is not installed! Please install ###"
  printf "%s\\n" "### and try again. ###"
fi

mkdir /root/.wireguard
privkeyPath="/root/.wireguard/privatekey"
pubkeyPath="/root/.wireguard/publickey"


wg genkey | tee $privkeyPath | \
  wg pubkey > $pubkeyPath
umask 077 $privkeyPath
umask 077 $pubkeyPath

printf "%s\\n" "### Generate private/public keypair for Wireguard ###"
privkey=$(<"$privkeyPath")

printf "%s\\n" "### Generate wireguard iface wg0 conf file ###"
cat >> /etc/wireguard/wg0.conf <<EOF

[Interface]
PrivateKey =$privkey
Address = 192.168.115.5/24
ListenPort = 51820
PostUp = iptables -A FORWARD -i wg0 -j ACCEPT; iptables -A FORWARD -o eth0 -j ACCEPT; iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE
PostDown = iptables -D FORWARD -i wg0 -j ACCEPT; iptables -D FORWARD -o eth0 -j ACCEPT; iptables -t nat -D POSTROUTING -o eth0 -j MASQUERADE
SaveConfig = true

# PJ's work notebook ASUS ROG
[Peer]
PublicKey =pywC2YikLn9YUe+z1syZc+ltTwn1wVK+jocsvRrd5S4=
AllowedIPs = 192.168.115.95/24
EOF

printf "%s\\n" "### Enable port forwarding ###"
echo "net.ipv4.ip_forward=1" >> /etc/sysctl.conf
sysctl -p
echo 1 > /proc/sys/net/ipv4/ip_forward

printf "%s\\n" "### Bring interface wg0 up ###"
wg-quick up wg0
printf "%s\\n" "### Start wg0 iface on boot ###"
systemctl enable wg-quick@wg0
printf "%s\\n" "### Show wg0 iface info ###"
wg show
ip a
