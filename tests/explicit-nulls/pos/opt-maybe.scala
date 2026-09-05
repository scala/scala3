
import language.experimental.errorHandling
import scala.util.{Ok, Err}

type Opt[+A] = A | Null
object Opt:

  def unapply[A](o: Opt[A]): A? =
    if o != null then Ok(o.asInstanceOf[o.type & A])
    else null

end Opt