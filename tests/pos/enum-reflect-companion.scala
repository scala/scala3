enum Foo1:
  case Baz, Bar

val check1 = summon[Foo1.type <:< scala.reflect.EnumCompanion[Foo1]]

//enum Foo2[T]:
//  case Baz extends Foo2[1]
//  case Bar extends Foo2[2]
//
//val check2 = summon[Foo2.type <:< scala.reflect.EnumCompanion[Foo2]]

enum Foo3:
  case Baz, Bar

object Foo3

val check3 = summon[Foo3.type <:< scala.reflect.EnumCompanion[Foo3]]
