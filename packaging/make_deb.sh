#! /bin/sh

VERSION=$1
INSTALL_DIR=$2
DATA_DIR=$3
DOC_DIR=$4

rm -rf packaging/dep2pict*.deb

mkdir -p packaging/deb/DEBIAN/
mkdir -p packaging/deb$DATA_DIR/src

cp packaging/dep2pict-$VERSION.tar.gz packaging/deb/$DATA_DIR/src


size="0"

cp packaging/DEBIAN/control packaging/deb/DEBIAN/control
cp packaging/DEBIAN/postinst packaging/deb/DEBIAN/postinst
cp packaging/DEBIAN/postrm packaging/deb/DEBIAN/postrm

chmod 755 packaging/deb/DEBIAN/postrm
chmod 755 packaging/deb/DEBIAN/postinst


sed -i "s|@VERSION@|$VERSION|" packaging/deb/DEBIAN/control
sed -i "s|@SIZE@|$size|" packaging/deb/DEBIAN/control
sed -i "s|@DATA_DIR@|$DATA_DIR|" packaging/deb/DEBIAN/postinst
sed -i "s|@VERSION@|$VERSION|" packaging/deb/DEBIAN/postinst

(cd packaging/ && dpkg-deb --build deb && cd -) || (echo "Fatal error : maybe the version number is null (make ... VERSION=...)" && exit 1)
mv packaging/deb.deb packaging/dep2pict-$VERSION-i386.deb
rm -rf packaging/deb
