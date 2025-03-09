case class City(name: String, population: Int)

def getCityInfo(city: City) = city match
  case City(iam = n, confused = p) => // error // error
    s"$n has a population of $p" // error // error

def getCityInfo1(city: Option[City]) = city match
  case Some(iam = n) => // error
    n // error