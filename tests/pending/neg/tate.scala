trait Out[+T]

object Test {

  def foo[T <% AnyRef](x: T) = ???

  var x: Out[_ >: String] = ???
  var y: Out[String] = ???
  x = y // should give error, but currently masked by covariant alias representation
 // y = x
}
