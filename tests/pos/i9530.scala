trait Food
case class Banana(color: String) extends Food

trait Diet[A <: Animal]:
  type F <: Food
  def food: Seq[F]

trait Animal
object Animal:
  extension [A <: Animal](using diet: Diet[A])(animal: A) def food1 = diet.food
  extension [A <: Animal](animal: A)(using diet: Diet[A]) def food2 = diet.food

extension [A <: Animal](using diet: Diet[A])(animal: A) def food3 = diet.food
extension [A <: Animal](animal: A)(using diet: Diet[A]) def food4 = diet.food

trait Monkey extends Animal

given Diet[Monkey]:
  type F = Banana
  def food: Seq[Banana] = Seq(new Banana("yellow"), Banana("green"))

trait FoodOps
given FoodOps:
  extension [A <: Animal](using diet: Diet[A])(animal: A) def food5 = diet.food
  extension [A <: Animal](animal: A)(using diet: Diet[A]) def food6 = diet.food


val monkey = new Monkey {}

val foods = Seq(
  monkey.food1,
  monkey.food2,
  monkey.food3,
  monkey.food4,
  monkey.food5,
  monkey.food6,
)
