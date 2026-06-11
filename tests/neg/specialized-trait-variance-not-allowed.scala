//> using options -language:experimental.specializedTraits

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

def sound(dogs: MyList[Dog]) = 
    dogs.map(_.makeNoise)

def throwAwayGeneralPaper(bin: Bin[Paper]) = 
    val a4 = Paper()
    bin.throwAway(a4)

def main =
    /* These ones are not allowed normally */
    val myAnimals: MyList[Animal] = new MyList(List(Dog(), Dog(), Lion())) {}
    sound(myAnimals) // error: MyList[Animal] can be interpreted as MyList[Dog] due to covariance

    val myNewspaperWasteBin = new Bin[Newspaper]() {}
    throwAwayGeneralPaper(myNewspaperWasteBin) // error: Bin[Newspaper] cannot be interpreted as Bin[Paper] due to contravariance

    /* These ones are specifically banned for specialized traits: */
    val myObjectBin = new Bin[Object]() {}
    throwAwayTheNewspaper(myObjectBin) // error: This use of contravariance is not compatible with specialized traits.

    val myAnyBin = new Bin[Any]() {}
    throwAwayTheNewspaper(myAnyBin) // error: This use of contravariance is not compatible with specialized traits.

    throwAwayAnInteger(myAnyBin) // error: This use of contravariance is not compatible with specialized traits.

    val myAnyRefBin = new Bin[AnyRef]() {}
    val myAnyValBin = new Bin[AnyVal]() {}
    
    throwAwayTheNewspaper(myAnyRefBin) // error: This use of contravariance is not compatible with specialized traits
    throwAwayAnInteger(myAnyValBin) // error: This use of contravariance is not compatible with specialized traits


