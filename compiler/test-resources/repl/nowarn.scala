scala> @annotation.nowarn def f = try 1 // @nowarn doesn't work on first line, ctx.run is null in issueIfNotSuppressed
-- Warning:
1 | @annotation.nowarn def f = try 1 // @nowarn doesn't work on first line, ctx.run is null in issueIfNotSuppressed
  |                            ^^^^^
  |                   A try without catch or finally is equivalent to putting
  |                   its body in a block; no exceptions are handled.
def f: Int
scala> @annotation.nowarn def f = try 1
def f: Int
scala> def f = try 1
-- Warning:
1 | def f = try 1
  |         ^^^^^
  |         A try without catch or finally is equivalent to putting
  |         its body in a block; no exceptions are handled.
def f: Int
scala> @annotation.nowarn def f = { 1; 2 }
def f: Int
scala> def f = { 1; 2 }
-- Warning:
1 | def f = { 1; 2 }
  |           ^
  |A pure expression does nothing in statement position; you may be omitting necessary parentheses
def f: Int
