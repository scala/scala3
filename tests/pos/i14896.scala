object Ex  { def unapply(p: Any): Option[_ <: Int] = null }
object Foo { val Ex(_) = null: @unchecked }