object Test {
type Meat = {
  type IsMeat = Any
}
type Grass = {
  type IsGrass = Any
}
trait Animal {
  type Food
  def eats(food: Food): Unit
  def gets: Food
}
type Cow = Animal {
  type IsMeat = Any
  type Food <: Grass
}
type Lion = Animal {
  type Food = Meat
}
def newMeat: Meat = new {
  type IsMeat = Any
}
def newGrass: Grass = new {
  type IsGrass = Any
}
def newCow: Cow = new Animal {
  type IsMeat = Any
  type Food = Grass
  def eats(food: Grass) = ()
  def gets = newGrass
}
def newLion: Lion = new Animal {
  type Food = Meat
  def eats(food: Meat) = ()
  def gets = newMeat
}
val milka = newCow
val leo = newLion
leo.eats(milka)
}
