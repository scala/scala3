// `FirstTransform.toTypeTree` creates `Annotated` nodes whose `annot` are
// `Ident`s, not annotation instances. This is relevant for `Checking.checkAnnot`.
//
// See also:
// - tests/run/t2755.scala
// - tests/neg/i13044.scala

def f(a: Array[?]) =
  a match
    case x: Array[?] => ()

def f2(t: Tuple) =
  t match
    case _: (t *: ts) => ()
    case _ => ()

