case class Ref(x: {*} String)

@annotation.capability class C
def test(c: C) =
  val x1 = Ref("hello")
  val y = x1 match
    case Ref(z) => z
