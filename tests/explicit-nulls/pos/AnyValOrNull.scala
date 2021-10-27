case class MyVal(i: Boolean) extends AnyVal

class Test1:

  def test1 =
    val v: AnyVal | Null = null
    if v == null then
      println("null")

  def test2 =
    val v: Int | Null = 1
    if v != null then
      println(v)

  def test3 =
    val v: MyVal | Null = MyVal(false)
    if v != null then
      println(v)

class Test2:
  import scala.language.unsafeNulls

  def test1 =
    val v: AnyVal | Null = null
    if v == null then
      println("null")

  def test2 =
    val v: Int | Null = 1
    if v != null then
      println(v)

  def test3 =
    val v: MyVal | Null = MyVal(false)
    if v != null then
      println(v)
