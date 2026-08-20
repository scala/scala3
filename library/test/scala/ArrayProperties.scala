package scala

import scala.reflect.ClassTag
import org.scalacheck.*
import Prop.*
import Gen.*
import Arbitrary.*
import util.*
import Buildable.*
import scala.util.Random

object ArrayProps extends Properties("Array") {
  implicit def arbArray[T](implicit a: Arbitrary[T], m: ClassTag[T]): Arbitrary[Array[T]] =
    Arbitrary(containerOf[List,T](arbitrary[T]) map (_.toArray))

  val arrGen: Gen[Array[?]] = oneOf(
    arbitrary[Array[Int]],
    arbitrary[Array[Array[Int]]],
    arbitrary[Array[List[String]]],
    arbitrary[Array[String]],
    arbitrary[Array[Boolean]],
    arbitrary[Array[AnyVal]]
  )

  // inspired by #1857 and #2352
  property("eq/ne") = forAll(arrGen, arrGen) { (c1, c2) =>
    (c1 eq c2) || (c1 ne c2)
  }

  // inspired by #2299
  def smallInt = choose(1, 10)
  property("ofDim") = forAll(smallInt, smallInt, smallInt) { (i1, i2, i3) =>
    val arr = Array.ofDim[String](i1, i2, i3)
    val flattened = arr.flatten.flatten
    flattened.length == i1 * i2 * i3
  }

  property("range") = forAll(
    Gen.choose(-1000, 1000),
    Gen.choose(-1000, 1000),
    Gen.oneOf(Gen.choose(1, 100), Gen.choose(-100, -1)),
  ) { (start, end, step) =>
    Array.range(start, end, step).toList == List.range(start, end, step)
  }

  property("iterate") = forAll(
    implicitly[Arbitrary[Int]].arbitrary,
    Gen.choose(-10, 100),
    implicitly[Arbitrary[Int => Int]].arbitrary,
  ) { (start, len, f) =>
    Array.iterate(start, len)(f).toList == List.iterate(start, len)(f)
  }

  property("fill") = forAll(
    Gen.choose(-10, 100),
  ) { len =>
    val xs = Vector.fill(len)(Random.nextInt())
    val i = xs.iterator
    Array.fill(len)(i.next()).toVector == xs
  }

  property("tabulate") = forAll(
    Gen.choose(-10, 100),
    implicitly[Arbitrary[Int => Int]].arbitrary,
  ) { (len, f) =>
    Array.tabulate(len)(f).toList == List.tabulate(len)(f)
  }

}
