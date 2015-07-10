trait T {
  private def foo = 0
  private[this] def bar = 0

}

class C { self: T =>
  foo                                     // error
  bar                                     // error
}

