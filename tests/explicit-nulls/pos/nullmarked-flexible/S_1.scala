// With flexible types, null-marked types are still exact, while repeated parameters
// stay usable even when the array is annotated as nullable.

import a.A

def test(a: A) =
  val s1: String = a.get()
  val s2: String | Null = a.getNullable()
  val n: String | Null = null
  a.varargs("a", "b")
  a.nullableElemVarargs("a", null, n)
  a.nullableVarargs("a", "b")
  a.nullableVarargs()
  a.nullableVarargs(Seq("a")*)
