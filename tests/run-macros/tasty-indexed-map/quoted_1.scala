
import scala.quoted.*

class MyMap[Keys](private val underlying: Array[Int]) extends AnyVal {
  def get[K <: String](implicit i: Index[K, Keys]): Int = underlying(i.index)
  def set[K <: String](value: Int)(implicit i: Index[K, Keys]): Unit = underlying(i.index) = value
}

object MyMap {
  def create[Keys](implicit s: Size[Keys]): MyMap[Keys] = new MyMap[Keys](new Array[Int](s.size))
}

trait Size[Keys] { def size: Int }
object Size {
  def apply[Keys](v: Int): Size[Keys] = new Size { def size = v }
  implicit val unit: Size[Unit] = Size(0)
  implicit def cons[H, T](implicit s: Size[T]): Size[(H, T)] = Size(s.size + 1)
}

class Index[K, Keys](val index: Int) extends AnyVal
object Index {

  implicit def zero[K, T]: Index[K, (K, T)] = new Index(0)

  implicit inline def succ[K, H, T](implicit prev: => Index[K, T]): Index[K, (H, T)] = ${succImpl[K, H, T]}

  def succImpl[K, H, T](implicit qctx: Quotes, k: Type[K], h: Type[H], t: Type[T]): Expr[Index[K, (H, T)]] = {
    import quotes.reflect.*

    def name(tp: TypeRepr): String = tp match {
      case ConstantType(StringConstant(str)) => str
    }

    def names(tp: TypeRepr): List[String] = tp match {
      case AppliedType(_, x1 :: x2 :: Nil) => name(x1) :: names(x2)
      case _ => Nil
    }

    val key = name(TypeRepr.of[K])
    val keys = name(TypeRepr.of[H]) :: names(TypeRepr.of[T])

    val index = keys.indexOf(key)

    '{new Index(${Expr(index)})}
  }
}
