object Ex  { def unapply(p: Any): Option[? <: Int] = null }
object Foo { val Ex(_) = null: @unchecked }