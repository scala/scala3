import example.Macro

class FooSmall[A, B] { type D; type C }
class FooLarge[A, B, C] { type E; type D }

type FooUnion[A, B] = FooSmall[A, B] | FooLarge[A, B, Int]
type FooAnd[A, B] = FooSmall[A, B] & FooLarge[A, B, Int]

trait CLS4[A] { type B4 }
trait CLS3[A] extends CLS4[A] { type B3; type A3 }
trait CLS2[A] { type B2 }
trait CLS1[A, B, C] extends CLS2[A] with CLS3[B] { type B1 }

trait SharedParent[A] { type Shared }
trait SharedA[A] extends SharedParent[A] { type B }
trait SharedB[A] extends SharedParent[A] { type C }
type SharedAnd1[A] = SharedA[A] & SharedB[A]
type SharedAnd2[A] = SharedB[A] & SharedA[A]
type SharedUnion[A] = SharedA[A] | SharedB[A]

@main def Test(): Unit = {
  println(Macro.typeMembers[FooSmall])
  println(Macro.typeMembers[FooLarge])

  println(Macro.typeMembers[FooUnion])
  println(Macro.typeMembers[FooAnd])

  println(Macro.typeMembers[CLS1])

  println(Macro.typeMembers[SharedAnd1])
  println(Macro.typeMembers[SharedAnd2])
  println(Macro.typeMembers[SharedUnion])
}
