package scala
import annotation.experimental

@experimental
object NamedTuple:

  opaque type Element[name <: String & Singleton, A] >: A = A

  object Element:
    def apply[S <: String & Singleton, A](name: S, x: A): Element[name.type, A] = x

    inline def unapply[S <: String & Singleton, A](named: Element[S, A]): Some[(S, A)] =
      Some((compiletime.constValue[S], named))

    def extract[S <: String & Singleton]: ValueExtractor[S] = ValueExtractor[S]()
    extension [S <: String & Singleton, A](named: Element[S, A]) def value: A = named

    class ValueExtractor[S <: String & Singleton]:
      def unapply[A](x: Element[S, A]): Some[A] = Some(x)
  end Element

  type DropNames[T <: Tuple] = T match
    case Element[_, x] *: xs => x *: DropNames[xs]
    case _ => T

  extension [T <: Tuple](x: T) def dropNames: DropNames[T] =
    x.asInstanceOf // named and unnamed tuples have the same runtime representation
end NamedTuple

