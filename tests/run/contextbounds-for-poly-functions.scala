import scala.language.experimental.modularity
import scala.language.future

trait Show[X]:
  def show(x: X): String

given Show[Int] with
  def show(x: Int) = x.toString

given Show[String] with
  def show(x: String) = x

case class Person(name: String, age: Int)

given Show[Person] with
  def show(x: Person) = s"${x.name} is ${x.age} years old"

type Shower = [X: Show] => X => String
val shower: Shower = [X: {Show as show}] => (x: X) => show.show(x)

type DoubleShower = [X: Show] => X => [Y: Show] => Y => String
val doubleShower: DoubleShower = [X: {Show as show1}] => (x: X) => [Y: {Show as show2}] => (y: Y) => s"${show1.show(x)} and ${show2.show(y)}"

object Test extends App:
  println(shower(42))
  println(shower("a string"))
  println(shower(Person("Kate", 27)))
  println(doubleShower(42)("a string"))
  println(doubleShower("a string")(Person("Kate", 27)))
  println(doubleShower(Person("Kate", 27))(42))
