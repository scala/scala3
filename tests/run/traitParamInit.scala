object Trace {
  private var results = List[Any]()
  def apply[A](a: A) = {results ::= a; a}
  def fetchAndClear(): Seq[Any] = try results.reverse finally results = Nil
}
trait T(a: Any) {
  val ta = a
  Trace(s"T.<init>($ta)")
  val t_val = Trace("T.val")
}

trait U(a: Any) extends T {
  val ua = a
  Trace(s"U.<init>($ua)")
}

object Test {
  def check(expected: Any) = {
    val actual = Trace.fetchAndClear()
    if (actual != expected)
      sys.error(s"\n$actual\n$expected")
  }
  def main(args: Array[String]): Unit = {
    new T(Trace("ta")) with U(Trace("ua")) {}
    check(List("ta", "T.<init>(ta)", "T.val", "ua", "U.<init>(ua)"))

    new U(Trace("ua")) with T(Trace("ta")) {}
    check(List("ta", "T.<init>(ta)", "T.val", "ua", "U.<init>(ua)"))
  }
}
