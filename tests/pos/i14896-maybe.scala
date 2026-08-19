//> using options -Yexplicit-nulls
import language.experimental.magic
object Ex  { def unapply(p: Any): (_ <: Int)? = null }
object Foo { val Ex(_) = null: @unchecked }