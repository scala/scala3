import Nat._

enum Nat:
  case Zero
  case Succ[N <: Nat](n: N)

enum MyEnum:
  case A[E <: Enum](e: E)

final case class Foo[T](t: T)

inline def willNotReduce1 = inline Foo(Zero) match // assert that enums are widened when the bound is not a parent enum type
  case Foo(Zero) => ()

inline def willNotReduce2 = inline MyEnum.A(Zero) match // assert that enums are only narrowed when bound is own enum type
  case MyEnum.A(Zero) => ()

val foo = willNotReduce1 // error: cannot reduce inline match with scrutinee: Foo.apply[Nat](Nat$#Zero) : Foo[Nat]
val bar = willNotReduce2 // error: cannot reduce inline match with scrutinee: MyEnum.A.apply[Nat](Nat$#Zero): MyEnum.A[Nat]
