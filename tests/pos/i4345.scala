import scala.annotation.tailrec
class Context {
  type Tree
}

class TestSimple {
  @tailrec
  final def loop(c: Context)(trees: List[c.Tree]): Boolean =
    loop(c)(trees)

  @tailrec
  final def loop2(c: Context, trees: List[c.Tree]): Boolean =
    loop2(c, trees)

  @tailrec
  final def loop3[A <: Context](c: A, trees: List[c.Tree]): Boolean =
    loop3(c, trees)
}

class TestVCParameterized[C <: Context](val classC: C) extends AnyVal {
  @tailrec
  final def loop(c: C)(trees: List[c.Tree]): List[c.Tree] =
    loop(c)(trees)

  @tailrec
  final def loop2(c: C, trees: List[c.Tree]): List[c.Tree] =
    loop2(c, trees)

  @tailrec
  final def loop3[A <: C](c: A, trees: List[c.Tree]): List[c.Tree] =
    loop3(c, trees)

  @tailrec
  final def loop4(trees: List[classC.Tree]): List[classC.Tree] =
    loop4(trees)

  def loopNonRec(c: C)(trees: List[c.Tree]): List[c.Tree] = {
    loopNonRec(c)(trees)
    loopNonRec(c)(trees)
  }

  def loopNonRec2(c: C, trees: List[c.Tree]): List[c.Tree] = {
    loopNonRec2(c, trees)
    loopNonRec2(c, trees)
  }

  def loopNonRec3[A <: Context](c: A, trees: List[c.Tree]): List[classC.Tree] = {
    loopNonRec3(c, trees)
    loopNonRec3(c, trees)
  }

  def loopNonRec4(trees: List[classC.Tree]): List[classC.Tree] = {
    loopNonRec4(trees)
    loopNonRec4(trees)
  }
}
