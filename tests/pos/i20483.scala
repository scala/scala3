
class Foo
  (x: Option[String])
  (using Boolean)
  (using Int)
  (using Double):

  def this
    (x: String)
    (using Boolean)
    (using Int)
    (using Double) =
    this(Some(x))