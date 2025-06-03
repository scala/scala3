//> using options -source future
import scala.compiletime.asMatchable

type City = (name: String, zip: Int, pop: Int)
type Raw = (String, Int, Int)

type Coord = (x: Double, y: Double)
type Labels = (x: String, y: String)

@main def Test =
  val city: City = (name = "Lausanne", zip = 1000, pop = 140000)
  val coord: Coord = (x = 1.0, y = 0.0)
  val labels: Labels = (x = "west", y = "north")

  val size: 3 = city.size
  assert(city.size == 3)

  val zip: Int = city(1)
  assert(zip == 1000)

  val name: String = city.head
  assert(name == "Lausanne")

  val zip_pop: (zip: Int, pop: Int) = city.tail
  val (_: Int, _: Int) = zip_pop
  assert(zip_pop == (zip = 1000, pop = 140000))

  val cinit = city.init
  val _: (name: String, zip: Int) = cinit
  assert(cinit == (name = "Lausanne", zip = 1000))

  val ctake1: (name: String) = city.take(1)
  assert(ctake1 == (name = "Lausanne"))

  val cdrop1 = city.drop(1)
  val _: (zip: Int, pop: Int) = cdrop1
  assert(cdrop1 == zip_pop)

  val cdrop3 = city.drop(3)
  val _: NamedTuple.Empty = cdrop3
  assert(cdrop3 == NamedTuple.Empty)

  val cdrop4 = city.drop(4)
  val _: NamedTuple.Empty = cdrop4
  assert(cdrop4 == NamedTuple.Empty)

  val csplit = city.splitAt(1)
  val _: ((name: String), (zip: Int, pop: Int)) = csplit
  assert(csplit == ((name = "Lausanne"), zip_pop))

  val city_coord = city ++ coord
  val _: NamedTuple.Concat[City, Coord] = city_coord
  val _: (name: String, zip: Int, pop: Int, x: Double, y: Double) = city_coord
  assert(city_coord == (name = "Lausanne", zip = 1000, pop = 140000, x = 1.0, y = 0.0))

  type IntToString[X] = X match
    case Int => String
    case _ => X

  val intToString = [X] => (x: X) => x.asMatchable match
    case x: Int => x.toString
    case x => x

  val citymap = city.map[IntToString](intToString.asInstanceOf)
  val _: (name: String, zip: String, pop: String) = citymap
  assert(citymap == (name = "Lausanne", zip = "1000", pop = "140000"))

  val cityreverse = city.reverse
  val _: (pop: Int, zip: Int, name: String) = cityreverse
  assert(cityreverse == (pop = 140000, zip = 1000, name = "Lausanne"))

  val zipped = coord.zip(labels)
  val _: (x: (Double, String), y: (Double, String)) = zipped
  val (x3, y3) = zipped
  val _: (Double, String) = x3
  assert(zipped == (x = (1.0, "west"), y = (0.0, "north")))

  val zippedRaw = ((1.0, "west"), (0.0, "north"))
  val (x1: (Double, String), x2: (Double, String)) = zippedRaw

  val cityFields = city.toList
  val _: List[String | Int] = cityFields
  assert(cityFields == List("Lausanne", 1000, 140000))

  val citArr = city.toArray
  val _: List[String | Int] = cityFields
  assert(cityFields == List("Lausanne", 1000, 140000))

