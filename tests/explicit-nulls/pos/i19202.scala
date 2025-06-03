class Test {
  def test1(s: String | Null): Unit = {
    if s == null then return

    case class XXX()
  }

  def test2(s: String | Null): Unit = {
    if s == "" then return

    case class XXX()
  }

  def test3(s: String | Null): Unit = {
    if s == null then return

    case class XXX()
    ()
  }

  def test4(s: String | Null): String | Null = {
    if s == null then return ""

    case class XXX()
    "xxx"
  }
}