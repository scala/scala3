//> using options -experimental

type City = (name: String, zip: Int, pop: Int)
type Coord = (x: Double, y: Double)
type Labels = (x: String, y: String)

@main def Test =
  val city: City = (name = "Lausanne", zip = 1000, pop = 140000)
  val coord: Coord = (x = 1.0, y = 0.0)
  val labels: Labels = (x = "west", y = "north")

  // first field updated
  val coord_update = coord.copyFrom((x = 2.0))
  val _: Coord = coord_update
  assert(coord_update.x == 2.0 && coord_update.y == 0.0)

  // last field updated
  val city_update = city.copyFrom((pop = 150000))
  val _: City = city_update
  assert(city_update.name == "Lausanne" && city_update.zip == 1000 && city_update.pop == 150000)

  // replace field types
  val coord_to_labels = coord.copyFrom((x = "east", y = "south"))
  val _: Labels = coord_to_labels
  assert(coord_to_labels.x == "east" && coord_to_labels.y == "south")

  // out of order
  val city_update2 = city.copyFrom((pop = 150000, name = "Lausanne", zip = 1000))
  val _: City = city_update2
