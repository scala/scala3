package example

enum Tag[T] with
  case Int extends Tag[Int]
  case String extends Tag[String]
  case OfClass[T]()(using val tag: reflect.ClassTag[T]) extends Tag[T]

enum ListLike[+T] with
  case Cons[T](head: T, tail: ListLike[T]) extends ListLike[T]
  case EmptyListLike
object ListLike with
  def valuef(s: String): ListLike[?] = ??? // this will usually trigger a "- did you mean ListLike.valuef" addendum

object Extensions with
  extension (foo: Nothing) // this will usually trigger an attempted extension method addendum
    def values: Array[Tag[?]] = ???

enum TypeCtorsK[F[_]] with
  case List       extends TypeCtorsK[List]
  case Option     extends TypeCtorsK[Option]
  case Const[T]() extends TypeCtorsK[[U] =>> T]

object UnimportedExtensions with
  extension (TypeCtorsKModule: TypeCtorsK.type) // this will usually trigger an import suggestions addendum
    def valueOf(name: String): TypeCtorsK[?] = ???

object NotAnEnum // object without a companion class

def Test: Unit =
  import Tag._, ListLike._, TypeCtorsK._, Extensions._

  val tags: Array[Tag[?]]              = Tag.values // error
  val listlikes: Array[ListLike[?]]    = ListLike.values // error
  val typeCtorsK: Array[TypeCtorsK[?]] = TypeCtorsK.values // error

  Tag.valueOf("Int") // error
  ListLike.valueOf("EmptyListLike") // error
  TypeCtorsK.valueOf("Option") // error

  NotAnEnum.values // error
  NotAnEnum.valueOf("Foo") // error
