import scala.reflect.{EnumCompanion, SingletonEnumCompanion}
enum Foo1:
  case Baz, Bar

val check1 = summon[Foo1.type <:< SingletonEnumCompanion[Foo1]]

enum Foo2[T]:
  case Baz extends Foo2[1]
  case Bar extends Foo2[2]

val check2 = summon[Foo2.type <:< SingletonEnumCompanion[Foo2[?]]]

enum Foo3[A, B[_]]:
  case Baz extends Foo3[Int, List]
  case Bar extends Foo3[Int, List]

val check3 = summon[Foo3.type <:< SingletonEnumCompanion[Foo3[?, ?]]]

extension [T <: reflect.Enum](enumCompanion : SingletonEnumCompanion[T])
  def check(arg : T) : Unit = assert(enumCompanion.values.contains(arg))

enum Foo4:
  case Yes
  case No(whyNot: String)
  case Skip

val check4 = summon[Foo4.type <:< EnumCompanion[Foo4]]

@main def Test : Unit =
  Foo3.check(Foo3.Bar)
  (Foo3 : AnyRef) match
    case _ : SingletonEnumCompanion[?] =>
    case _ : EnumCompanion[?] => assert(false)
    case _  => assert(false)

  (Foo4 : AnyRef) match
    case _ : SingletonEnumCompanion[?] => assert(false)
    case _ : EnumCompanion[?] =>
    case _  => assert(false)

enum Foo5:
  case Baz, Bar

trait Hello
object Foo5 extends Hello

//TODO: fix implementation so this would work
//val check5 = summon[Foo5.type <:< scala.reflect.EnumCompanion[Foo5] with Hello]
