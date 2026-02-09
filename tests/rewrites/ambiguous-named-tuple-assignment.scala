object i21770:
  def f(g: Int  => Unit) = g(0)
  var cache: Option[Int] = None
  f(i => (cache = Some(i)))

object i21861:
  var age: Int = 28
  (
    age = 29
  )


object i21861c:
  def age: Int = ???
  def age_=(x: Int): Unit = ()
  age = 29
  ( age = 29 )
