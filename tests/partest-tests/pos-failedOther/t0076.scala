object bug {
  def foo(i: => Int): Int = 0;

  def bar: Int = {
    var i: Int = 0;
    foo (i);
  }
}
