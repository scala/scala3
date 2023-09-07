import scala.deriving.Mirror
import scala.reflect.ClassTag

transparent inline given derived[A](
      using m: Mirror.ProductOf[A],
      idClassTag: ClassTag[Tuple.Union[m.MirroredElemTypes]]
  ): Unit = ???

case class Foo(a: Int)

val instance = derived[Foo] // error
