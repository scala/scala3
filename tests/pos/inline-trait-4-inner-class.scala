inline trait Options[T]:
  // FIXME: remove original class definition from inline trait to allow public classes
  private trait Option:
    def get: T
  // FIXME: support constructor parameter
  // FIXME: support specialized parents
  private class Some://(x: T): // extends Option
    def get: T = ??? // x
  // FIXME: support specialized modules
  // private object None // extends Option
  //   def get: T = throw new NoSuchElementException("None.get")

  // FIXME: specialize reference to Option and Some
  // def option(value: T): Option = new Some//(value)
end Options

object IntOptions extends Options[Int]:
  /*
  <generated> trait Option:
    def get: Int
  <generated> class Some(x: Int) extends Option
    def get: Int = x
  <generated> object None extends Option
    def get: Int = throw new NoSuchElementException("None.get")

  <generated> def option(value: Int): Option = new Some(value)
  */
end IntOptions
