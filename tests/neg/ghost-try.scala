object Test {
  def foo(ghost a: Int): Int = {
    try {
      a // error
    } catch {
      case _ => 42
    }
  }
  def foo2(ghost a: Int): Int = {
    try {
      42
    } catch {
      case _ => a // error
    }
  }
}
