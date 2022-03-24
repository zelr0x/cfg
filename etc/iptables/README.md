# iptables firewall configuration
## Introduction
See [archwiki article](https://wiki.archlinux.org/title/Simple_stateful_firewall).

## Apply rules safely
```shell
iptables-apply iptables.rules
ip6tables-apply ip6tables.rules
```

## Persist rules
```shell
sudo apt install iptables-persistent
```

