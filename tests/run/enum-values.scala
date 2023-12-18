// scalajs: --skip

import reflect.Selectable.reflectiveSelectable
import deriving.Mirror

enum Color:
  case Red, Green, Blue

enum Suits extends java.lang.Enum[Suits]:
  case Clubs, Spades, Diamonds, Hearts

enum Tag[T]:
  case Int extends Tag[Int]
  case OfClass[T]()(using val tag: reflect.ClassTag[T]) extends Tag[T] // mix order of class and value
  case String extends Tag[String]

enum Expr[-T >: Null]:
  case EmptyTree extends Expr[Null]
  case AnyTree

enum ListLike[+T]:
  case Cons[T](head: T, tail: ListLike[T]) extends ListLike[T]
  case EmptyListLike

enum TypeCtorsK[F[_]]:
  case List       extends TypeCtorsK[List]
  case Const[T]() extends TypeCtorsK[[U] =>> T] // mix order of class and value
  case Option     extends TypeCtorsK[Option]

enum MixedParams[F[_], G[X,Y] <: collection.Map[X,Y], T]:
  case Foo extends MixedParams[List, collection.mutable.LinkedHashMap, Unit]

enum ClassOnly: // this should still generate the `ordinal` and `fromOrdinal` companion methods
  case BranchProd(i: Int)

@main def Test: Unit =
  import Color._, Suits._, Tag._, Expr._, ListLike._, TypeCtorsK._, MixedParams._, ClassOnly.*

  type FromOrdinal[T] = {
    def fromOrdinal(ordinal: Int): T
  }

  type ValueOf[T] = {
    def valueOf(s: String): T
  }

  extension [A](t: A) def show = runtime.ScalaRunTime.stringOf(t)

  def fetchFromOrdinal[T <: AnyRef & reflect.Enum](companion: FromOrdinal[T], compare: T*): Unit =
    for c <- compare do
      assert(companion.fromOrdinal(c.ordinal) eq c,
        s"$c does not `eq` companion.fromOrdinal(${c.ordinal}), got ${companion.fromOrdinal(c.ordinal)}")

  def notFromOrdinal[T <: AnyRef & reflect.Enum](companion: FromOrdinal[T], compare: T): Unit =
    cantFind(companion.asInstanceOf[FromOrdinal[Any]], compare.ordinal)

  def cantFind[T](companion: FromOrdinal[T], ordinal: Int): Unit =
    try
      companion.fromOrdinal(ordinal)
      throw new AssertionError(s"$companion.fromOrdinal(${ordinal}) did not fail")
    catch
      case e: java.lang.reflect.InvocationTargetException => // TODO: maybe reflect.Selectable should catch this?
        assert(e.getCause.isInstanceOf[java.util.NoSuchElementException]
          && e.getCause.getMessage == ordinal.toString)

  fetchFromOrdinal(companion = Color,       compare = Red, Green, Blue)
  fetchFromOrdinal(companion = Suits,       compare = Clubs, Spades, Diamonds, Hearts)
  fetchFromOrdinal(companion = Tag,         compare = Int, String)
  fetchFromOrdinal(companion = Expr,        compare = EmptyTree, AnyTree)
  fetchFromOrdinal(companion = ListLike,    compare = EmptyListLike)
  fetchFromOrdinal(companion = TypeCtorsK,  compare = List, Option)
  fetchFromOrdinal(companion = MixedParams, compare = Foo)

  notFromOrdinal(companion = Tag,        compare = OfClass[String]())
  notFromOrdinal(companion = TypeCtorsK, compare = Const[String]())
  notFromOrdinal(companion = ClassOnly,  compare = BranchProd(1)) // ClassOnly has the `fromOrdinal` method

  cantFind(companion = Color,     ordinal = 500) // test default case for enumeration
  cantFind(companion = Suits,     ordinal = 500) // test default case for Java style enumeration
  cantFind(companion = Tag,       ordinal = 500) // test default case for mixed adt with non-simple values
  cantFind(companion = ClassOnly, ordinal = 500) // should always throw

  assert(summon[Mirror.SumOf[ClassOnly]].ordinal(BranchProd(1)) == 0)

  val colors: Array[Color] = Color.values
  val exprs: Array[Expr[? >: Null]] = Expr.values
  val mixedParams: Array[MixedParams[?, ? <: [X, Y] =>> collection.Map[X, Y], ?]] = MixedParams.values

  def sameAs[T](arr: Array[T], compare: T*): Unit =
    assert(arr sameElements compare, s"${arr.show} does not correspond to ${compare.show}")

  sameAs(colors,      Red, Green, Blue)
  sameAs(exprs,       EmptyTree, AnyTree)
  sameAs(mixedParams, Foo)

  def singleton[E <: AnyRef](value: E, name: String, companion: ValueOf[E]) =
    val lookup = companion.valueOf(name)
    assert(value eq lookup, s"${value.show} is not identical to ${lookup.show}")

  singleton(Green, "Green", Color)
  singleton(AnyTree, "AnyTree", Expr)
  singleton(Foo, "Foo", MixedParams)
