class X

object X {
  extension (x: List[X]) { def isNull = x.head == null }
}

@main def Test =
  assert(List(null: X).isNull)
  assert(List((null: X)).isNull)
