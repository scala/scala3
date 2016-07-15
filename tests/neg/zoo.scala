object Test {
type Meat = {
  type IsMeat = Any
}
type Grass = {
  type IsGrass = Any
}
type Animal = {
  type Food
  def eats(food: Food): Unit   // error
  def gets: Food               // error
}
type Cow = {
  type IsMeat = Any
  type Food <: Grass
  def eats(food: Grass): Unit  // error
  def gets: Grass              // error
}
type Lion = {
  type Food = Meat
  def eats(food: Meat): Unit   // error
  def gets: Meat               // error
}
def newMeat: Meat = new {
  type IsMeat = Any
}
def newGrass: Grass = new {
  type IsGrass = Any
}
def newCow: Cow = new {
  type IsMeat = Any
  type Food = Grass
  def eats(food: Grass) = ()
  def gets = newGrass
}
def newLion: Lion = new {
  type Food = Meat
  def eats(food: Meat) = ()
  def gets = newMeat
}
val milka = newCow
val leo = newLion
leo.eats(milka) // structural select not supported
}
