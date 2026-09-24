//> using options -Ycheck:all
package foo
package object G:
  private[foo] val valBinaryAPI2: Int = 1
package object H:
  inline def inlined = G.valBinaryAPI2 // was -Ycheck failure
