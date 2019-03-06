object Test {
  type T1[X] = X match {
    case 1 => Int
    case 2 => String
  }

  identity[T1[1]](1)
  identity[T1[2]]("")
  identity[T1[3]]("") // error
  identity[T1[3]](1) // error
  identity[T1[Int]]("") // error
  identity[T1[Int]](1) // error

  type T2[X] = X match {
    case 1 => Int
    case _ => String
  }

  identity[T2[1]](1)
  identity[T2[2]]("")
  identity[T2[Int]]("") // error
  identity[T2[2]](1) // error
  identity[T2[Int]](1) // error

  sealed trait A
  final class B extends A
  final class C extends A

  type T3[X] = X match {
    case B => Int
    case C => String
  }

  identity[T3[B]](1)
  identity[T3[C]]("")
  identity[T3[A]](1) // error
  identity[T3[A]]("") // error

  type T4[X] = X match {
    case A => Int
    case C => String
  }

  identity[T4[B]](1)
  identity[T4[C]](1)
  identity[T4[A]](1)

  type T5[X] = X match {
    case C => String
    case A => Int
  }

  identity[T5[C]]("")
  identity[T5[B]](1)
  identity[T5[A]](1) // error
  identity[T5[A]]("") // error

  class D

  type T6[X] = X match {
    case A => Int
    case D => String
  }

  identity[T6[A]](1)
  identity[T6[B]](1)
  identity[T6[C]](1)
  identity[T6[D]]("")

  trait A2
  final class B2 extends A2
  final class C2 extends A2

  type T7[X] = X match {
    case A2 => Int
    case D => String
  }

  identity[T7[A2]](1)
  identity[T7[B2]](1)
  identity[T7[C2]](1)
  identity[T7[D]]("") // error
  identity[T7[D]](1) // error

  trait E1
  trait E2

  type T8[X] = X match {
    case E1 => Int
    case E2 => String
  }

  identity[T8[E1]](1)
  identity[T8[E2]](1) // error
  identity[T8[E1]]("") // error
  identity[T8[E2]]("") // error

  type T9[X] = X match {
    case Tuple2[Int, String] => Int
    case Tuple2[String, Int] => String
  }

  identity[T9[Tuple2[Int, String]]](1)
  identity[T9[Tuple2[String, Int]]]("1")
  identity[T9[Tuple2[Nothing, String]]](1)
  identity[T9[Tuple2[String, Nothing]]]("1")
  identity[T9[Tuple2[Int, Nothing]]](1)
  identity[T9[Tuple2[Nothing, Int]]]("1")
  identity[T9[Tuple2[_, _]]]("") // error
  identity[T9[Tuple2[_, _]]](1) // error
  identity[T9[Tuple2[Any, Any]]]("") // error
  identity[T9[Tuple2[Any, Any]]](1) // error

  case class Box2[+A, +B, +C](a: A, b: B)

  type TA[X] = X match {
    case Box2[Int, Int, Int] => Int
    case Box2[Int, Int, String] => String
  }

  identity[TA[Box2[Int, Int, Int]]](1)
  identity[TA[Box2[Int, Int, String]]](1) // error
  identity[TA[Box2[Int, Int, String]]]("") // error

  case class Box2_I[A, B, C](a: A, b: B)

  type TC[X] = X match {
    case Box2_I[Int, Int, Int] => Int
    case Box2_I[Int, Int, String] => String
  }

  identity[TC[Box2_I[Int, Int, Int]]](1)
  identity[TC[Box2_I[Int, Int, String]]]("")


  case class Box2_C[A, B, -C](a: A, b: B)

  type TD[X] = X match {
    case Box2_C[Int, Int, Int] => Int
    case Box2_C[Int, Int, String] => String
  }

  identity[TD[Box2_C[Int, Int, Int]]](1)
  identity[TD[Box2_C[Int, Int, String]]]("") // error
}

object Test2 {
  type M[A] = A match {
    case Option[Int] => String
    case Some[_]     => Int
  }

  def a[A]: M[Some[A]] = 1  // error
  def b[A]: M[Some[A]] = "" // error
}

object Test3 {
  trait Inv[T]

  type M[A] = A match {
    case Inv[Int] => String
    case _ => Int
  }

  def test[A]: Unit = {
    // We need to be careful here, we cannot trust the output of type
    // comparer on `isSameType(A, Int)` since `A` is a type parameter...
    val a: M[Inv[A]] = 1 // error
    ()
  }
}

object Test4 {
  trait Inv[T]

  type M[A] = A match {
    case Inv[Int] => String
    case _ => Int
  }

  class Foo {
    type A

    def test: Unit = {
      // We need to be careful here, we cannot trust the output of type
      // comparer on `isSameType(A, Int)` since `A` is an abstract type.
      val a: M[Inv[A]] = 1 // error
      ()
    }
  }
}
