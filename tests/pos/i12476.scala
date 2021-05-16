object test {
  def foo[A, B](m: B) = {
    m match {
      case _: A =>
        m match {
          case _: B =>  // crash with -Yno-deep-subtypes
        }
    }
  }
}
