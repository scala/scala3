//> using options -Yexplicit-nulls
import language.experimental.magic
object Ex  { def unapply(p: Any): (? <: Int)? = null } // error
object Foo { val Ex(_) = null: @unchecked }