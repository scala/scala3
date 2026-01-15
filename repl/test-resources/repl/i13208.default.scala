scala> try 1
1 warning found
-- [E002] Syntax Warning: ------------------------------------------------------
1 | try 1
  | ^^^^^
  | A try without catch or finally is equivalent to putting
  | its body in a block; no exceptions are handled.
  |
  | longer explanation available when compiling with `-explain`
val res0: Int = 1
