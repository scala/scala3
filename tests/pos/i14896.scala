object Ex  { def unapply(p: Any): Option[_ <: Int] = ??? }
object Foo { val Ex(_) = null: @unchecked }