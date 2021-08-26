import scala.compiletime.{summonFrom, summonInline, erasedValue}
import scala.deriving.Mirror

trait EnumerateNames[T]

object EnumerateNames {
  inline def derived[T]: EnumerateNames[T] =
    summonFrom {
      case ev: Mirror.Of[T] =>
        inline ev match
          case m: Mirror.ProductOf[T] => ???
          case m: Mirror.SumOf[T] =>
            inline erasedValue[m.MirroredElemTypes] match
              case _: (tpe *: _) => summonInline[EnumerateNames[tpe]]
              case _: EmptyTuple =>
        ???
    }
}

class MainClass {
  enum Shape:
    case Point
  inline given auto[T]: EnumerateNames[T] = EnumerateNames.derived[T]
  def shapeNames: EnumerateNames[Shape] = EnumerateNames.derived[Shape]
}
