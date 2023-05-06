
object Test:

  def void(): Unit = ???
  def self(): this.type = this

  def test(): Unit =
    this // is line 8
      .self()
      .void()

  @annotation.nowarn
  def main(args: Array[String]): Unit =
    try test()
    catch (t: Throwable) =>
      t.getStackTrace.iterator
        .filter(_.getFileName == "apply-line.scala")
        .filter(_.getMethodName == "test")
        .foreach(sf => assert(sf.getLineNumber == 10))

/* was
Test$.void(apply-line.scala:4)
Test$.test(apply-line.scala:8)
Test$.main(apply-line.scala:14)
Test.main(apply-line.scala)
*/
