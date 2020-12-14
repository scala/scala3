
case class i0 (i0: i1) extends AnyVal // error
trait i1 extends i0   // error

trait F[x] extends AnyVal    // error
case class G[x](a: F[x]) extends F[x] // error // error
