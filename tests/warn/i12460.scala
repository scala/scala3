//> using options -explain

extension (s: String = "hello, world") def invert = s.reverse.toUpperCase // warn

extension (using String)(s: String = "hello, world") def revert = s.reverse.toUpperCase // warn

extension (s: String) def divert(m: String  = "hello, world") = (s+m).reverse.toUpperCase // ok
