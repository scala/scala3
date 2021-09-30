scala> @annotation.nowarn def f = try 1 // @nowarn doesn't work on first line, ctx.run is null in issueIfNotSuppressed
1 warning found
-- [E000] Syntax Warning: ------------------------------------------------------
1 | @annotation.nowarn def f = try 1 // @nowarn doesn't work on first line, ctx.run is null in issueIfNotSuppressed
  |                            ^^^^^
  |                   A try without catch or finally is equivalent to putting
  |                   its body in a block; no exceptions are handled.
longer explanation available when compiling with `-explain`
def f: Int
scala> @annotation.nowarn def f = try 1
def f: Int
scala> def f = try 1
1 warning found
-- [E000] Syntax Warning: ------------------------------------------------------
1 | def f = try 1
  |         ^^^^^
  |         A try without catch or finally is equivalent to putting
  |         its body in a block; no exceptions are handled.
longer explanation available when compiling with `-explain`
def f: Int
scala> @annotation.nowarn def f = { 1; 2 }
def f: Int
scala> def f = { 1; 2 }
1 warning found
-- [E129] Potential Issue Warning: ---------------------------------------------
1 | def f = { 1; 2 }
  |           ^
  |A pure expression does nothing in statement position; you may be omitting necessary parentheses
longer explanation available when compiling with `-explain`
def f: Int
