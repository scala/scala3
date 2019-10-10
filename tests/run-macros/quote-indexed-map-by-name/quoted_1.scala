import scala.quoted._

class Index[K, Keys](val index: Int) extends AnyVal
object Index {

  implicit def zero[K, T]: Index[K, (K, T)] = new Index(0)

  implicit inline def succ[K, H, T](implicit prev: => Index[K, T]): Index[K, (H, T)] = ${succImpl('[K], '[H], '[T])}

  def succImpl[K, H, T](k: TypeTag[K], h: TypeTag[H], t: TypeTag[T])(given QuoteContext): Expr[Index[K, (H, T)]] = {
    implicit val kk: TypeTag[K] = k
    implicit val hh: TypeTag[H] = h
    implicit val tt: TypeTag[T] = t
    '{new Index(0)}
  }
}
