#!/bin/env sh

# Installs Oracle VirtualBox on Debian-based distributions.
# Change the following constants if needed.
# Tested on ubuntu 22 (jammy).

DISTR="$(lsb_release -cs)"  # distribution codename e.g. "jammy" for Ubuntu 22
ARCH="amd64"
APT_KEYSTORE_PUBKEY_ID="info@virtualbox.org"
PUBKEY_URL="https://www.virtualbox.org/download/oracle_vbox_2016.asc"
PUBKEY_FILE="/usr/share/keyrings/oracle-vbox-2016.gpg"
SOURCES_FILE="/etc/apt/sources.list.d/oracle-vbox.list"
SOURCE="deb [arch=$ARCH signed-by=$PUBKEY_FILE] https://download.virtualbox.org/virtualbox/debian $DISTR contrib"
PACKAGE="virtualbox"
APT_CMD="sudo apt update && sudo apt install $PACKAGE"

# Download signing key, dearmor (convert from b64) and save.
echo "Downloading public key from $PUBKEY_URL..."
wget -q -O- "${PUBKEY_URL}" | gpg --dearmor -o "$PUBKEY_FILE"
res=$?
if [ "$res" -ne 0 ]; then
	return "$res" 2>/dev/null
	exit "$res"
fi
echo "Created $PUBKEY_FILE."

# Basically chmod +r for _apt user. Probably not needed.
# sudo setfacl -m u:_apt:r "$PUBKEY_FILE"

# Delete key from deprecated store if present.
if [[ ! -z "${APT_KEYSTORE_PUBKEY_ID}" ]]; then
	echo "Deleting the key from the deprecated apt store..."
	sudo apt-key del "${APT_KEYSTORE_PUBKEY_ID}" 2>/dev/null
fi

if [ -f "$SOURCES_FILE" ]; then
	echo "File $SOURCES_FILE already exists."
	echo "Edit it to add the following line resolving any conflicts along the way:"
	echo "${SOURCE}"
	echo "Then run the following line:"
	echo "${APT_CMD}"
	return 1 2>/dev/null
	exit 1
fi
echo "Adding the repository to $SOURCES_FILE..."
sudo echo "${SOURCE}" > "$SOURCES_FILE"

read -p "apt is prepared to install $PACKAGE, proceed with install? (y/n) " yn
case "$yn" in
	[Yy]*) eval "${APT_CMD}" ;;
	*) echo "Aborting. Run ${APT_CMD} to finish the installation." ;;
esac

