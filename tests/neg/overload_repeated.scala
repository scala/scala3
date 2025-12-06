object Test {
  def bar1(x: Any) = 1
  def bar1(x: String*) = 2

  // (b) isn't as good as (a) because of ints
  // (a) isn't as good as (b) because T in Class[? <: T] may not subtype of Number
  def bar2[T](a: Class[? <: T]): Int = 1 // (a)
  def bar2[T <: Number](a: Class[T], ints: Int*): Int = 2 // (b)

  assert(bar1("") == 1) // error: ambiguous in Scala 2 and Scala 3
  assert(bar2(classOf[Integer]) == 1) // error: ambiguous in Scala 2 and Scala 3
}
