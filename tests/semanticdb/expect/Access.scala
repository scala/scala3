package example

class Access {
  private def m1 = ???
  private[this] def m2 = ???
  private[Access] def m3 = ???
  protected def m4 = ???
  protected[this] def m5 = ???
  protected[example] def m6 = ???
  def m7 = ???
}
