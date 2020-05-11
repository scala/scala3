import scala.quoted._

class Index[K, Keys](val index: Int) extends AnyVal
object Index {

  implicit def zero[K, T]: Index[K, (K, T)] = new Index(0)

  implicit inline def succ[K, H, T](implicit prev: => Index[K, T]): Index[K, (H, T)] = ${succImpl('[K], '[H], '[T])}

  def succImpl[K, H, T](using s: Scope)(k: s.Type[K], h: s.Type[H], t: s.Type[T]): s.Expr[Index[K, (H, T)]] = {
    implicit val kk: s.Type[K] = k
    implicit val hh: s.Type[H] = h
    implicit val tt: s.Type[T] = t
    '{new Index(0)}
  }
}
