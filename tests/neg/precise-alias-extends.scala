import annotation.precise

object preciseTypeAliasExtendSamePrecise:
  trait FooInv[@precise T]:
    val value: T
  class FooInvExtend[@precise A](val value : A) extends FooInv[A]
  type FooInvAlias[@precise A] = FooInv[A]
  opaque type FooInvOpaque[@precise A] = FooInv[A]

  trait FooCov[@precise +T]:
    val value: T
  class FooCovExtend[@precise +A](val value : A) extends FooCov[A]
  type FooCovAlias[@precise +A] = FooCov[A]
  opaque type FooCovOpaque[@precise +A] = FooCov[A]

  trait FooCon[@precise -T]
  class FooConExtend[@precise -A] extends FooCon[A]
  type FooConAlias[@precise -A] = FooCon[A]
  opaque type FooConOpaque[@precise -A] = FooCon[A]


object preciseTypeAliasExtendMorePrecise:
  trait FooInv[T]:
    val value: T
  class FooInvExtend[@precise A](val value : A) extends FooInv[A]
  type FooInvAlias[@precise A] = FooInv[A] // error
  opaque type FooInvOpaque[@precise A] = FooInv[A] // error

  trait FooCov[+T]:
    val value: T
  class FooCovExtend[@precise +A](val value : A) extends FooCov[A]
  type FooCovAlias[@precise +A] = FooCov[A] // error
  opaque type FooCovOpaque[@precise +A] = FooCov[A] // error

  trait FooCon[-T]
  class FooConExtend[@precise -A] extends FooCon[A]
  type FooConAlias[@precise -A] = FooCon[A] // error
  opaque type FooConOpaque[@precise -A] = FooCon[A] // error


object preciseTypeAliasExtendLessPrecise:
  trait FooInv[@precise T]:
    val value: T
  class FooInvExtend[A](val value : A) extends FooInv[A] // error
  type FooInvAlias[A] = FooInv[A]
  opaque type FooInvOpaque[A] = FooInv[A] // error

  trait FooCov[@precise +T]:
    val value: T
  class FooCovExtend[+A](val value : A) extends FooCov[A] // error
  type FooCovAlias[+A] = FooCov[A]
  opaque type FooCovOpaque[+A] = FooCov[A] // error

  trait FooCon[@precise -T]
  class FooConExtend[-A] extends FooCon[A] // error
  type FooConAlias[-A] = FooCon[A]
  opaque type FooConOpaque[-A] = FooCon[A] // error


object preciseTypeAliasComposition:
  trait Foo[@precise T]
  trait Box[T]

  type FooAlias1[A] = Box[Foo[A]]
  type FooAlias2[A] = Foo[Box[A]]

  type BoxAlias1[@precise A] = Box[Foo[A]]
  type BoxAlias2[@precise A] = Foo[Box[A]] // error


object preciseTypeBounds:
  class Box[T]
  class PBox[@precise T]

  object Alias:
    object Upper:
      type Same[@precise T] <: PBox[T]
      type Less[T] <: PBox[T] // error
      type More[@precise T] <: Box[T]
    object Lower:
      type Same[@precise T] >: PBox[T]
      type Less[T] >: PBox[T]
      type More[@precise T] >: Box[T] // error

  object Opaque:
    opaque type Same[@precise T] <: PBox[T] = PBox[T]
    opaque type Less[T] <: PBox[T] = PBox[T] // error
    opaque type More[@precise T] <: Box[T] = Box[T] // error


object preciseGivenWith:
  class PreciseBox[@precise T]
  given [T]: PreciseBox[T] with {}