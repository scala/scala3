class Test {
  def test1(s: String): Unit = {
    if s == null then return

    case class XXX()
  }

  def test2(s: String): Unit = {
    if s == "" then return

    case class XXX()
  }

  def test3(s: String): Unit = {
    if s == null then return

    case class XXX()
    ()
  }

  def test4(s: String): String = {
    if s == null then return ""

    case class XXX()
    "xxx"
  }
}