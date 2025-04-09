#!/usr/bin/env fish

for file in (ls library/src/**.scala | sort)
  set checkbox (if rg -q 'import (scala.)?language.experimental.captureChecking' $file; echo "[x]"; else; echo "[ ]"; end)
  printf "- %s %s\n" $checkbox $file
end
