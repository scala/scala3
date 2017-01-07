import scala.reflect.Selectable.reflectiveSelectable
object Test {
type Meat = {
  type IsMeat = Any
}
type Grass = {
  type IsGrass = Any
}
type Animal = {
  type Food
  def eats(food: Food): Unit
  def gets: Food
}
type Cow = {
  type IsMeat = Any
  type Food <: Grass
  def eats(food: Grass): Unit
  def gets: Grass
}
type Lion = {
  type Food = Meat
  def eats(food: Meat): Unit
  def gets: Meat
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
leo.eats(milka)
}
