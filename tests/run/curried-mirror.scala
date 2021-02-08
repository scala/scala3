import scala.deriving.*

object Test extends App {
  case class Prod0(i: Int)

  {
    val v0 = summon[Mirror.Product { type MirroredType = [T] =>> Prod0 }]
    val v1 = v0.fromProduct(Tuple1(13))
    val v2: Prod0 = v1
    assert(v2 == Prod0(13))
  }

  {
    val v0 = summon[Mirror.Product { type MirroredType = [T, U] =>> Prod0 }]
    val v1 = v0.fromProduct(Tuple1(13))
    val v2: Prod0 = v1
    assert(v2 == Prod0(13))
  }

  case class Prod1[A](a: A)

  {
    val v0 = summon[Mirror.Product { type MirroredType = [T, U] =>> Prod1[U] }]
    val v1 = v0.fromProduct(Tuple1(13))
    val v2: Prod1[_] = v1
    assert(v2 == Prod1(13))
  }

  case class Prod2[A, B](a: A, b: B)

  {
    val v0 = summon[Mirror.Product { type MirroredType[X, Y] = Prod2[X, Y] }]
    val v1 = v0.fromProduct((23, "foo"))
    val v2: Prod2[_, _] = v1
    assert(v2 == Prod2(23, "foo"))
  }

  {
    val v0 = summon[Mirror.Product { type MirroredType = [B] =>> Prod2[Int, B] }]
    val v1 = v0.fromProduct((23, "foo"))
    val v2: Prod2[Int, _] = v1
    assert(v2 == Prod2(23, "foo"))
  }

  {
    val v0 = summon[Mirror.Product { type MirroredType = Prod2[Int, String] }]
    val v1 = v0.fromProduct((23, "foo"))
    val v2: Prod2[Int, String] = v1
    assert(v2 == Prod2(23, "foo"))
  }

  case class ProdV1[+A](a: A)

  {
    val v0 = summon[Mirror.Product { type MirroredType = [T, U] =>> ProdV1[U] }]
    val v1 = v0.fromProduct(Tuple1(13))
    val v2: ProdV1[_] = v1
    assert(v2 == ProdV1(13))
  }

  case class ProdV2[+A, +B](a: A, b: B)

  {
    val v0 = summon[Mirror.Product { type MirroredType[+X, +Y] = ProdV2[X, Y] }]
    val v1 = v0.fromProduct((23, "foo"))
    val v2: ProdV2[_, _] = v1
    assert(v2 == ProdV2(23, "foo"))
  }

  {
    val v0 = summon[Mirror.Product { type MirroredType = [B] =>> ProdV2[Int, B] }]
    val v1 = v0.fromProduct((23, "foo"))
    val v2: ProdV2[Int, _] = v1
    assert(v2 == ProdV2(23, "foo"))
  }

  {
    val v0 = summon[Mirror.Product { type MirroredType = ProdV2[Int, String] }]
    val v1 = v0.fromProduct((23, "foo"))
    val v2: ProdV2[Int, String] = v1
    assert(v2 == ProdV2(23, "foo"))
  }

  sealed trait Sum0
  object Sum0 {
    case class Some(i: Int) extends Sum0
    case object None extends Sum0
  }

  {
    val v0 = summon[Mirror.Sum { type MirroredType = [T] =>> Sum0 }]
    val v1 = v0.ordinal(Sum0.Some(13))
    assert(v1 == 0)
    val v2 = v0.ordinal(Sum0.None)
    assert(v2 == 1)
  }

  {
    val v0 = summon[Mirror.Sum { type MirroredType = [T, U] =>> Sum0 }]
    val v1 = v0.ordinal(Sum0.Some(13))
    assert(v1 == 0)
    val v2 = v0.ordinal(Sum0.None)
    assert(v2 == 1)
  }

  sealed trait Sum1[A]
  object Sum1 {
    case class Some[A](a: A) extends Sum1[A]
    case class None[A]() extends Sum1[A]
  }

  {
    val v0 = summon[Mirror.Sum { type MirroredType = [T, U] =>> Sum1[U] }]
    val v1 = v0.ordinal(Sum1.Some(13))
    assert(v1 == 0)
    val v2 = v0.ordinal(Sum1.None())
    assert(v2 == 1)
  }

  sealed trait Sum2[A, B]
  object Sum2 {
    case class Left[A, B](a: A) extends Sum2[A, B]
    case class Right[A, B](b: B) extends Sum2[A, B]
  }

  {
    val v0 = summon[Mirror.Sum { type MirroredType[X, Y] = Sum2[X, Y] }]
    val v1 = v0.ordinal(Sum2.Left(23))
    assert(v1 == 0)
    val v2 = v0.ordinal(Sum2.Right("foo"))
    assert(v2 == 1)
  }

  {
    val v0 = summon[Mirror.Sum { type MirroredType = [B] =>> Sum2[Int, B] }]
    val v1 = v0.ordinal(Sum2.Left(23))
    assert(v1 == 0)
    val v2 = v0.ordinal(Sum2.Right("foo"))
    assert(v2 == 1)
  }

  {
    val v0 = summon[Mirror.Sum { type MirroredType = Sum2[Int, String] }]
    val v1 = v0.ordinal(Sum2.Left(23))
    assert(v1 == 0)
    val v2 = v0.ordinal(Sum2.Right("foo"))
    assert(v2 == 1)
  }

  sealed trait SumV1[+A]
  object SumV1 {
    case class Some[A](a: A) extends SumV1[A]
    case object None extends SumV1[Nothing]
  }

  {
    val v0 = summon[Mirror.Sum { type MirroredType = [T, U] =>> SumV1[U] }]
    val v1 = v0.ordinal(SumV1.Some(13))
    assert(v1 == 0)
    val v2 = v0.ordinal(SumV1.None)
    assert(v2 == 1)
  }

  sealed trait SumV2[+A, +B]
  object SumV2 {
    case class Left[A](a: A) extends SumV2[A, Nothing]
    case class Right[B](b: B) extends SumV2[Nothing, B]
  }

  {
    val v0 = summon[Mirror.Sum { type MirroredType[+X, +Y] = SumV2[X, Y] }]
    val v1 = v0.ordinal(SumV2.Left(23))
    assert(v1 == 0)
    val v2 = v0.ordinal(SumV2.Right("foo"))
    assert(v2 == 1)
  }

  {
    val v0 = summon[Mirror.Sum { type MirroredType = [B] =>> SumV2[Int, B] }]
    val v1 = v0.ordinal(SumV2.Left(23))
    assert(v1 == 0)
    val v2 = v0.ordinal(SumV2.Right("foo"))
    assert(v2 == 1)
  }

  {
    val v0 = summon[Mirror.Sum { type MirroredType = SumV2[Int, String] }]
    val v1 = v0.ordinal(SumV2.Left(23))
    assert(v1 == 0)
    val v2 = v0.ordinal(SumV2.Right("foo"))
    assert(v2 == 1)
  }
}
