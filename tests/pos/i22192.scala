case class City(name: String, population: Int)

def getCityInfo(city: City) = city match
  case City(population = p, name = n) =>
    s"$n has a population of $p"

def getCityInfo1(city: Option[(name: String)]) = city match
  case Some(name = n) => n
  case _ =>

def getCityInfo2(city: Option[(name: String, population: Int)]) = city match
  case Some(name = n) => n
  case _ =>
