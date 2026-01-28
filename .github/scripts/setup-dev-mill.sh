#!/usr/bin/env bash
set -e

if [ ! -e "$HOME/.ivy2/local/mill-proguard.jar" ]; then
  MILL_COMMIT="de17c958012"

  # Adding an extra directory "thing", to work around some Mill issue with scripts
  mkdir thing
  cd thing
  git clone https://github.com/alexarchambault/mill.git -b scala3-new mill-scala3
  cd mill-scala3
  git switch --detach "$MILL_COMMIT"
  ./mill -i dist.installLocal
  mv mill-proguard.jar "$HOME/.ivy2/local/"
  cd ..
  cd ..
  rm -rf thing
fi

ln -sf "$HOME/.ivy2/local/mill-proguard.jar" mill
chmod +x mill
