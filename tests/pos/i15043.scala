import scala.deriving.Mirror

case class Size3(a1:Int, a2:Int, a3:Int)
case class Size22(a1:Int, a2:Int, a3:Int, a4:Int, a5:Int, a6:Int, a7:Int, a8:Int, a9:Int, a10:Int, a11:Int, a12:Int, a13:Int, a14:Int, a15:Int, a16:Int, a17:Int, a18:Int, a19:Int, a20:Int, a21:Int, a22:Int)
case class Size23(a1:Int, a2:Int, a3:Int, a4:Int, a5:Int, a6:Int, a7:Int, a8:Int, a9:Int, a10:Int, a11:Int, a12:Int, a13:Int, a14:Int, a15:Int, a16:Int, a17:Int, a18:Int, a19:Int, a20:Int, a21:Int, a22:Int, a23:Int)

type FieldValuePair[X <:Tuple] <: Tuple = X match {
  case EmptyTuple => X
  case t *: ts => t *: FieldValuePair[ts]
}

inline def baz[A](using m: Mirror.ProductOf[A])(a: Tuple.Zip[m.MirroredElemLabels, FieldValuePair[m.MirroredElemTypes]]) = a

@main def main =
  val s3 = baz[Size3]("a1" -> 1, "a2" -> 2, "a3" ->3 )

  val s22 = baz[Size22]("a1" -> 1, "a2" -> 2, "a3" -> 3, "a4" -> 4, "a5" -> 5, "a6" -> 6, "a7" -> 7,
  "a8" -> 8, "a9" -> 9, "a10" -> 10, "a11" -> 11, "a12" -> 12, "a13" -> 13, "a14" -> 14, "a15" -> 15,
  "a16" -> 16, "a17" -> 17, "a18" -> 18, "a19" -> 19, "a20" -> 20, "a21" -> 21, "a22" -> 22)


  val s23 = baz[Size23]("a1" -> 1, "a2" -> 2, "a3" -> 3, "a4" -> 4, "a5" -> 5, "a6" -> 6, "a7" -> 7,
    "a8" -> 8, "a9" -> 9, "a10" -> 10, "a11" -> 11, "a12" -> 12, "a13" -> 13, "a14" -> 14, "a15" -> 15,
    "a16" -> 16, "a17" -> 17, "a18" -> 18, "a19" -> 19, "a20" -> 20, "a21" -> 21, "a22" -> 22, "a23" -> 23)

