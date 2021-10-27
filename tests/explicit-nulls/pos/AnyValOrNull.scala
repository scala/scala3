def test1 =
  val v: AnyVal | Null = null
  if v == null then
    println("null")

def test2 =
  val v: Int | Null = 1
  if v != null then
    println(v)

case class MyVal(i: Boolean) extends AnyVal

def test3 =
  val v: MyVal | Null = MyVal(false)
  if v != null then
    println(v)
