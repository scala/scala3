enum Tag[T]:
  case Int extends Tag[Int]
  case String extends Tag[String]
  case OfClass[T]()(using val tag: reflect.ClassTag[T]) extends Tag[T]

enum ListLike[+T]:
  case Cons[T](head: T, tail: ListLike[T]) extends ListLike[T]
  case EmptyListLike

enum TypeCtorsK[F[_]]:
  case List       extends TypeCtorsK[List]
  case Option     extends TypeCtorsK[Option]
  case Const[T]() extends TypeCtorsK[[U] =>> T]

def Test: Unit =
  import Tag._, ListLike._, TypeCtorsK._
  import reflect.Selectable.reflectiveSelectable

  val tags: Array[Tag[?]]              = Tag.values // error
  val listlikes: Array[ListLike[?]]    = ListLike.values // error
  val typeCtorsK: Array[TypeCtorsK[?]] = TypeCtorsK.values // error

  Tag.valueOf("Int") // error
  ListLike.valueOf("EmptyListLike") // error
  TypeCtorsK.valueOf("Option") // error
