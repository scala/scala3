#!/usr/bin/env bash
TARGETS="$(pwd) /__w/_temp /github/home/"
for folder in $TARGETS; do
  echo "Cleaning $folder"
  cd $folder
  for f in $(ls -a); do
    if [ "$f" != "." ] &&
       [ "$f" != ".." ] &&
       [ "$f" != "_github_home" ]  # __w/_temp/_github_home is mount as
                                   # /github/home and is cleaned separately
    then
      rm -rf "$f"
    fi
  done
done
