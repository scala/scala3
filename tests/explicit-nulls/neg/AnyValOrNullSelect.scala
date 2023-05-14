case class MyVal(i: Int) extends AnyVal:
  def printVal: Unit =
    println(i)

class Test:
  val v: MyVal | Null = MyVal(1)

  def f1 =
    v.printVal // error: value printVal is not a member of MyVal | Null

  def f1 =
    import scala.language.unsafeNulls
    v.printVal // error: value printVal is not a member of MyVal | Null
