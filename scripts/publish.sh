#!/bin/bash
PRG=`readlink "$0"`
ROOT=`dirname "$PRG"`
cd "$ROOT"

if [ -z "$HACKAGE_USER" -o -z "$HACKAGE_PASS" ]; then
  echo "HACKAGE_USER or HACKAGE_PASS not set"
  exit 1
fi

update=`git diff-index --name-status HEAD | wc -l`

if [ "$update" -gt 0 ]; then
  echo "git has uncommitted modifications"
  exit 1
fi

function action(){
  stack haddock
  pkg=.
  stack sdist $pkg && stack upload $pkg
  hup docboth -u $HACKAGE_USER -p $HACKAGE_PASS 
}

for module in `ls -d yam*`; do 
  (cd $ROOT/$module; action)
done
