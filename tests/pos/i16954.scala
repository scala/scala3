class Test:
  def test =
    classOf[Test]

  def blck =
    class Blck
    val cls = classOf[Blck]
    cls

  def expr =
    class Expr
    classOf[Expr] // was: "assertion failed: leak: Expr in { [..] }" crash

object Test extends Test:
  def main(args: Array[String]): Unit =
    assert(test.getName == "Test",        test.getName)
    assert(blck.getName == "Test$Blck$1", blck.getName)
    assert(expr.getName == "Test$Expr$1", expr.getName)
