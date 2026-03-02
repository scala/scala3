//> using options -Werror -source:future -experimental

object Foo {

  val xs: Option[Int] = Some(1)

  def test: Int =
    xs.runtimeChecked match { // this test asserts that reachability is not avoided by runtimeChecked
      case is: Some[t] => is.get
      case is: Some[t] => ??? // unreachable
    }
}
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
