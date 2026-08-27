//> using options -language:experimental.specializedTraits

import scala.annotation.nowarn

trait Animal:
    def makeNoise: String
class Lion extends Animal:
    override def makeNoise = "ROAR!"
class Dog extends Animal:
    override def makeNoise: String = "BARK!"

trait Material
class Paper extends Material
class Newspaper extends Paper

inline trait MyList[+T: Specialized](val xs: List[T]):
    def map[S](f: T => S) = xs.map(f)

inline trait Bin[-T: Specialized]:
    def throwAway(x: T) = println(s"Throwing away ${x}")

def sound(animals: MyList[Animal]) = 
    animals.map(_.makeNoise)

def throwAwayTheNewspaper(bin: Bin[Newspaper]) = 
    val newspaper = Newspaper()
    bin.throwAway(newspaper)

def throwAwayAnInteger(bin: Bin[Int]) = 
    val integer = 100
    bin.throwAway(integer)

def throwAwayAnObject(bin: Bin[Object]) = 
    val obj = "good morning"
    bin.throwAway(obj)

def throwAwayAnAnyVal(bin: Bin[AnyVal]) = 
    val bc: AnyVal = 400
    bin.throwAway(bc)

@main def Test =
    val myDogs: MyList[Dog] = new MyList(List(Dog(), Dog(), Dog())) {}
    sound(myDogs) // MyList[Dog] can be interpreted as MyList[Animal] due to covariance

    val myWastepaperBasket = new Bin[Paper]() {}
    throwAwayTheNewspaper(myWastepaperBasket) // Bin[Paper] can be interpreted as Bin[Newspaper] due to contravariance

    // Fine; same erasure bucket
    val myAnyBin = new Bin[Any] {}
    val myAnyRefBin = new Bin[Any] {}
    throwAwayAnObject(myAnyBin)
    throwAwayAnObject(myAnyRefBin)
    throwAwayAnAnyVal(myAnyBin)
