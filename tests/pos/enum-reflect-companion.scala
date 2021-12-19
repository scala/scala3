import scala.reflect.EnumCompanion
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

//trait Hello
//object Foo3 extends Hello
//val x = Foo3.Bar
//val check3 = summon[Foo3.type <:< scala.reflect.EnumCompanion[Foo3]]

extension [T <: reflect.Enum](enumCompanion : EnumCompanion[T])
  def check(arg : T) : Unit = println(enumCompanion.values.map(_.ordinal).mkString("\n"))

@main def main : Unit =
  Foo3.check(Foo3.Bar)