import scala.quoted.*

class Index[K, Keys](val index: Int) extends AnyVal
object Index {

  implicit def zero[K, T]: Index[K, (K, T)] = new Index(0)

  implicit inline def succ[K, H, T](implicit prev: => Index[K, T]): Index[K, (H, T)] = ${succImpl(Type.of[K], Type.of[H], Type.of[T])}

  def succImpl[K, H, T](k: Type[K], h: Type[H], t: Type[T])(using Quotes): Expr[Index[K, (H, T)]] = {
    implicit val kk: Type[K] = k
    implicit val hh: Type[H] = h
    implicit val tt: Type[T] = t
    '{new Index(0)}
  }
}
