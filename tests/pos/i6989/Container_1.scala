package mypkg

object Container {
  class StringExtras(val s: String) extends AnyVal {
    def op(item: Int): Int = ???
  }
}

trait Container {
  import Container.*
  implicit def mkStringExtras(s: String): StringExtras = new StringExtras(s)
}