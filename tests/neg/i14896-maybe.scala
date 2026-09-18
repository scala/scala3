//> using options -Yexplicit-nulls
import language.experimental.errorHandling
object Ex  { def unapply(p: Any): (? <: Int)? = null } // error
object Foo { val Ex(_) = null: @unchecked }