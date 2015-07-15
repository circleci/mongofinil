#!/bin/bash

set -euo pipefail

VERSION=$1
MONGODIR=mongo-$VERSION

mkdir -p data-$VERSION

curl -O https://fastdl.mongodb.org/linux/mongodb-linux-x86_64-$VERSION.tgz
tar zxvf mongodb-linux-x86_64-$VERSION.tgz
mv mongodb-linux-x86_64-$VERSION $MONGODIR

$MONGODIR/bin/mongod --config scripts/mongo${VERSION}.conf --fork
