#!/usr/bin/env fish

for file in (ls library/src/**.scala | sort)
  set checkbox (if rg -Fq "import language.experimental.captureChecking" $file; echo "[x]"; else; echo "[ ]"; end)
  printf "- %s %s\n" $checkbox $file
end
