sealed trait A
case object B extends A

object Test:
  def before[T <: A](using Int)(t: T): Int = 1
  def before[T <: A](using Int)(t: T, b: Boolean): String = ""

  given Int = 0

  val a: Int = before(B)
  val b: String = before(B, true) // was: type mismatch: found (B.type, Boolean), required A

  // Same without the upper bound: used to compile but silently picked the
  // unary overload by auto-tupling the arguments.
  def g[T](using Int)(t: T): Int = 1
  def g[T](using Int)(t: T, b: Boolean): String = ""

  val c: Int = g(B)
  val d: String = g(B, true)

  // Auto-tupling still applies when all alternatives take a single
  // explicit parameter after the using clause.
  def h[T](using Int)(t: T): Int = 1
  val e: Int = h(B, true)
