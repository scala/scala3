import annotation.precise

object preciseDefOverride:
  object SamePrecise:
    abstract class Foo:
      def id[@precise T](t: T) : T
    class Bar extends Foo:
      def id[@precise T](t: T): T = ???

  object MorePrecise:
    abstract class Foo:
      def id[T](t: T) : T
    class Bar extends Foo:
      def id[@precise T](t: T): T = ??? // error

  object LessPrecise:
    abstract class Foo:
      def id[@precise T](t: T) : T
    class Bar extends Foo:
      def id[T](t: T): T = ??? // error


object preciseTypeAliasOverride:
  object SamePrecise:
    trait Foo:
      type Box[@precise T]
    class Bar extends Foo:
      class Box[@precise T]

  object LessPrecise:
    trait Foo:
      type Box[@precise T]
    class Bar extends Foo:
      class Box[T] // error

  object MorePrecise:
    trait Foo:
      type Box[T]
    class Bar extends Foo:
      class Box[@precise T] // error