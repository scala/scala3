class Animal
class Cat extends Animal
class Dog extends Animal

trait Cov[+X]
trait CovCat  extends Cov[Cat]
class CovBoth extends CovCat with Cov[Dog] // ok // val x: Cov[Dog] = CovBoth(); if x.is[CovCat] then x.as[CovCat].getCat
//         baseCls = Cov
//          middle = CovCat
//            self = CovBoth
//         superBT = CovCat
//      combinedBT = Cov[Cat]
// withoutMiddleBT = Cov[Dog]
// Comparing the arguments of `Cov[Cat]` and `Cov[Dog]`,
// `Cat` isn't a type argument of the super base type `CovCat`
// so we can ignore it.

object CollectionStrawMan4:
  trait Iter[+CC[_]]
  trait Seq[+A] extends Iter[Seq]
  class List[+A] extends Seq[A] with Iter[List] // ok // val x: Iter[List] = List[Int](); if x.is[Seq[?]] then x.as[Seq[?]]
  class Cons[+A] extends List[A]                // ok // val x: Iter[List] = Cons[Int](); if x.is[Seq[?]] then x.as[Seq[?]]

object CollectionStrawMan5:
  trait Build[+X, +C[X] <: Iter[X]]
  trait Iter[+Y] extends Build[Y, Iter]
  class List[+Z] extends Iter[Z] with Build[Z, List] // ok // val x: Build[Int, Iter] = List[Int](); if x.is[Iter[Int]] then x.as[Iter[Int]]

object PolyInheritanceCase:
  trait IOps[+A, +CC[_], +C]
  class Iter[+A] extends IOps[A, Iter, Iter[A]]
  class List[A] extends Iter[A] with IOps[A, List, List[A]] // ok // val x: IOps[Int, List, List[Int]] = List[Int](); if x.is[Iter[Int]] then x.as[Iter[Int]]

trait Get[+X]
class GetAnimal extends Get[Animal]
class GetCat    extends GetAnimal, Get[Cat]

class Test:
  def test[X](get: Get[X]): X = get match
    case _: GetAnimal => // X >: Animal (wrong!)
      new Dog // error: Found: Dog; Required: X

  def cat: Cat = test(new GetCat) // was: ClassCastException
