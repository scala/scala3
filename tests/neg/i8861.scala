object Test {
  sealed trait Container { s =>
    type A
    def visit[R](int: IntV & s.type => R, str: StrV & s.type => R): R
  }
  final class IntV extends Container { s =>
    type A = Int
    val i: Int = 42
    def visit[R](int: IntV & s.type => R, str: StrV & s.type => R): R = int(this)
  }
  final class StrV extends Container { s =>
    type A = String
    val t: String = "hello"
    def visit[R](int: IntV & s.type => R, str: StrV & s.type => R): R = str(this)
  }

  def minimalOk[R](c: Container { type A = R }): R = c.visit[R](
    int = vi => vi.i : vi.A,
    str = vs => vs.t : vs.A
  )
  def minimalFail[M](c: Container { type A = M }): M = c.visit(
    int = vi => vi.i : vi.A,
    str = vs => vs.t : vs.A  // error
  )

  def main(args: Array[String]): Unit = {
    val e: Container { type A = String } = new StrV
    println(minimalOk(e)) // this one prints "hello"
    println(minimalFail(e)) // this one fails with ClassCastException: class java.lang.String cannot be cast to class java.lang.Integer
  }
}