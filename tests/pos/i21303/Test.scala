import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.ClassTag
import scala.annotation.implicitNotFound


trait TSType[T]
object TSType extends DefaultTSTypes with TSTypeMacros

trait TSNamedType[T] extends TSType[T]

trait DefaultTSTypes extends JavaTSTypes
trait JavaTSTypes {
  given javaEnumTSType[E <: java.lang.Enum[E]: ClassTag]: TSType[E] = ???
}
object DefaultTSTypes extends DefaultTSTypes
trait TSTypeMacros {
  inline given [T: Mirror.Of]: TSType[T] = derived[T]
  inline def derived[T](using m: Mirror.Of[T]): TSType[T] = {
    val elemInstances = summonAll[m.MirroredElemTypes]
    ???
  }

  private inline def summonAll[T <: Tuple]: List[TSType[_]] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[TSType[t]] :: summonAll[ts]
    }
  }
}

@main def Test = summon[TSType[JavaEnum]]