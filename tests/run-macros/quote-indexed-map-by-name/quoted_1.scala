import scala.quoted._

class Index[K, Keys](val index: Int) extends AnyVal
object Index {

  implicit def zero[K, T]: Index[K, (K, T)] = new Index(0)

  implicit inline def succ[K, H, T](implicit prev: => Index[K, T]): Index[K, (H, T)] = ${succImpl('[K], '[H], '[T])}

  def succImpl[K, H, T](k: Staged[K], h: Staged[H], t: Staged[T])(using QuoteContext): Expr[Index[K, (H, T)]] = {
    implicit val kk: Staged[K] = k
    implicit val hh: Staged[H] = h
    implicit val tt: Staged[T] = t
    '{new Index(0)}
  }
}
