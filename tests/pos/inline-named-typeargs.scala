// Working version of inline-named-typedargs, the original is currently disabled
object t1 {
  inline def construct[Elem, Coll[_]](xs: List[Elem]) <: Coll[Elem] = ???
}
