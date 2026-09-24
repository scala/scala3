import scala.language.unsafeNulls
def test =
  Test.foo[Seq[Int], SomeTypeclass]
