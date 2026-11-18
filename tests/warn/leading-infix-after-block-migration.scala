//> using options -source:future-migration

def c = true

def indented =
  if c then
    10
  else
    1
  + 100 // warn

def braces =
  if c then {
    10
  } else {
    1
  }
  + 100 // warn
