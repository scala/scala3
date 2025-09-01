import compiletime.{summonFrom, erasedValue}

import deriving.Mirror

object EnumerateNames {

  inline def summonNext[T] =
    summonFrom {
      case en: EnumerateNames[T] => en
    }

  inline def walkThrough[Types <: Tuple]: List[String] =
    inline erasedValue[Types] match
      case _: (tpe *: tpes) =>
        summonNext[tpe].apply +: walkThrough[tpes]
      case _: EmptyTuple =>
        Nil


  inline def derived[T]: EnumerateNames[T] =
    summonFrom {
      case ev: Mirror.Of[T] =>
        new EnumerateNames[T] {
          def apply =
            inline ev match
              case m: Mirror.ProductOf[T] => walkThrough[m.MirroredElemTypes].mkString(", ")
              case m: Mirror.SumOf[T] => walkThrough[m.MirroredElemTypes].mkString(", ")
        }
    }
}

trait EnumerateNames[T] {
  def apply: String
}

class MainClass {
  enum Shape:
    case Square(width: Int, height: Int) extends Shape
    case Circle(radius: Int) extends Shape

  given EnumerateNames[Int] {
    def apply: String = "int"
  }
  inline given auto[T]:EnumerateNames[T] = EnumerateNames.derived
  def deriveEnumerateNames[T](using en: EnumerateNames[T]) = en.apply
  def run: Unit = assert( deriveEnumerateNames[Shape] == "int, int, int" )
}

@main def Test = new MainClass().run