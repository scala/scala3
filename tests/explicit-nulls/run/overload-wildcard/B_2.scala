object Test:

  // The formal parameter types come from Java: (Class[? <: T])? and (Class[T])?
  def formalFromJava(): Unit =
    assert(A_1.foo5(classOf[Object]: Class[? <: Object]) == 1)
    assert(A_1.foo10(new java.util.HashMap(): java.util.Map[String, ? <: Object]) == 1)

  // The argument type comes from Java: (java.util.Map[String, ? <: Object])?
  def bar[V](a: java.util.Map[String, ? <: V]): Int = 1
  def bar[V](a: java.util.Map[String, V], ints: Int*): Int = 2

  def argFromJava(): Unit =
    assert(bar(A_1.map()) == 1)

  def main(args: Array[String]): Unit =
    formalFromJava()
    argFromJava()
