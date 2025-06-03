//> using options -Werror -source:future -experimental

object Foo {

  val xs: List[Any] = List(1: Any)

  def test: Int =
    xs.runtimeChecked match { // this test asserts that unsound type tests still require @unchecked
      // tests/run/runtimeChecked.scala adds @unchecked to the
      // unsound type test to avoid the warning.
      case is: ::[Int/* can not be checked so still err */] => is.head
    }
}
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
