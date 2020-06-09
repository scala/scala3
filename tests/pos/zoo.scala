object Test {
trait FoodStuff
trait Meat extends FoodStuff {
  type IsMeat = Any
}
trait Grass extends FoodStuff {
  type IsGrass = Any
}
trait Animal {
  type Food <: FoodStuff
  def eats(food: Food): Unit
  def gets: Food
}
trait Cow extends Animal {
  type IsMeat = Any
  type Food <: Grass
  def eats(food: Food): Unit
  def gets: Food
}
trait Lion extends Animal {
  type Food = Meat
  def eats(food: Meat): Unit
  def gets: Meat
}
def newMeat: Meat = new Meat {
}
def newGrass: Grass = new Grass {
}
def newCow: Cow = new Cow {
  type Food = Grass
  def eats(food: Grass) = ()
  def gets = newGrass
}
def newLion: Lion = new Lion {
  def eats(food: Meat) = ()
  def gets = newMeat
}
val milka = newCow
val leo = newLion
//leo.eats(milka) // structural select not supported
}
