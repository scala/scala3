import language.experimental.captureChecking

trait A

case class B(x: AnyRef^) extends A

def test =
  val x: AnyRef^ = new AnyRef
  val a: A^{x} = B(x)

  val y1: A = a match
    case b: B => b // error: (b: B) becomes B^{x} implicitly

  val y2: A^{x} = a match
    case b: B =>
      val bb: B^{b} = b
      val aa: A^{a} = bb
      b // ok

  val x3: AnyRef = a match
    case B(x2: AnyRef) => x2 // error: we lose some information about field x, but it still cannot be pure

  val x4: AnyRef = a match
    case b: B => b.x // error
