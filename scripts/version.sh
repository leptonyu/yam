#!/bin/bash
PRG=`readlink "$0"`
ROOT=`dirname "$PRG"`
cd "$ROOT"

if echo "$1" | grep -vq '[0-9][0-9]*\(\.[0-9][0-9]*\)*' ; then
  echo "version not set"
  exit 1
fi

for pkg in `ls -d yam*`; do
  sed -i.bak "s|^version:\(  *\)[0-9][0-9]*\(\.[0-9][0-9]*\)*|version:\1$1|" $pkg/package.yaml
  rm -f $pkg/package.yaml.bak
done
