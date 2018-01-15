#!/bin/sh
PRG="$0"
# Need this for relative symlinks.
while [ -h "$PRG" ] ; do
    ls=`ls -ld "$PRG"`
    link=`expr "$ls" : '.*-> \(.*\)$'`
    if expr "$link" : '/.*' > /dev/null; then
        PRG="$link"
    else
        PRG=`dirname "$PRG"`"/$link"
    fi
done

ROOT=`dirname "$PRG"`
ROOT=`cd "$ROOT/..";pwd`

cd "$ROOT"

if [ -z "$HACKAGE_USER" -o -z "$HACKAGE_PASS" ]; then
  echo "HACKAGE_USER or HACKAGE_PASS not set"
  exit 1
fi

stack haddock

ls | grep yam- | while read pkg; do
  stack sdist $pkg && stack upload $pkg
  cd $pkg
  hup docboth -u $HACKAGE_USER -p $HACKAGE_PASS
  cd "$ROOT"
done

