
object Run {
  def test(): Unit = {
    Test[Foo[String]](Foo("moo"))
  }
}
