//> using options -explain -Ystop-after:refchecks

extension (s: String = "hello, world") def invert : String = s.reverse.toUpperCase // warn

extension (using String)(s: String = "hello, world") def revert : String = s.reverse.toUpperCase // warn

extension (s: String)
  def divert(m: String = "hello, world") : String = (s+m).reverse.toUpperCase // ok
  def divertimento(using String)(m: String = "hello, world") : String = (s+m).reverse.toUpperCase // ok
