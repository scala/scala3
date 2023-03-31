// The starting example of the kind of thing that
// GadtExpr should be able to handle without the help
// of GADT cast insertions and re-inferring GADT constraints all the time.
object Test extends Lib:
  def main(args: Array[String]): Unit =
    val expr: Expr[Int] = IntExpr()
    val int: Int = extract(expr)
    assert(int + 1 == 2)
