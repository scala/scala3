object Foo:
  def foo1: Int = {
    private def bar: Int = 0 // error
    bar
  }

  def foo2: Int = {
    private def bar: Int = 0 // error
    bar
  }

  def foo3: Int = {
    private def bar: Int = 0 // error
    bar
  }

  def foo4: Int = {
    private def bar: Int = 0 // error
    bar
  }
