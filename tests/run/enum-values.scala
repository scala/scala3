enum Color:
  case Red, Green, Blue

enum Tag[T]:
  case Int extends Tag[Int]
  case String extends Tag[String]
  case OfClass[T]()(using val tag: reflect.ClassTag[T]) extends Tag[T]

enum Expr[-T >: Null]:
  case EmptyTree extends Expr[Null]
  case AnyTree

enum ListLike[+T]:
  case Cons(head: T, tail: ListLike[T])
  case EmptyListLike

enum TypeCtorsK[F[_]]:
  case List       extends TypeCtorsK[List]
  case Option     extends TypeCtorsK[Option]
  case Const[T]() extends TypeCtorsK[[U] =>> T]

enum MixedParams[F[_], G[X,Y] <: collection.Map[X,Y], T]:
  case Foo extends MixedParams[List, collection.mutable.LinkedHashMap, Unit]

@main def Test: Unit =
  import Color._, Tag._, Expr._, ListLike._, TypeCtorsK._, MixedParams._
  import reflect.Selectable.reflectiveSelectable

  extension [A](t: A) def show = runtime.ScalaRunTime.stringOf(t)

  val colors: Array[Color]             = Color.values
  val tags: Array[Tag[?]]              = Tag.values
  val exprs: Array[Expr[? >: Null]]    = Expr.values
  val listlikes: Array[ListLike[?]]    = ListLike.values
  val typeCtorsK: Array[TypeCtorsK[?]] = TypeCtorsK.values

  val mixedParams: Array[MixedParams[?, ? <: [X, Y] =>> collection.Map[X, Y], ?]] = MixedParams.values

  def sameAs[T](arr: Array[T], compare: T*): Unit =
    assert(arr sameElements compare, s"${arr.show} does not correspond to ${compare.show}")

  sameAs(colors,      Red, Green, Blue)
  sameAs(tags,        Int, String)
  sameAs(exprs,       EmptyTree, AnyTree)
  sameAs(listlikes,   EmptyListLike)
  sameAs(typeCtorsK,  List, Option)
  sameAs(mixedParams, Foo)

  def singleton[E <: AnyRef](value: E, name: String, companion: { def valueOf(s: String): E}) =
    val lookup = companion.valueOf(name)
    assert(value eq lookup, s"${value.show} is not identical to ${lookup.show}")

  singleton(Green, "Green", Color)
  singleton(String, "String", Tag)
  singleton(AnyTree, "AnyTree", Expr)
  singleton(EmptyListLike, "EmptyListLike", ListLike)
  singleton(Option, "Option", TypeCtorsK)
  singleton(Foo, "Foo", MixedParams)
