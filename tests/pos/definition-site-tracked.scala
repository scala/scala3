import scala.language.experimental.modularity

tracked trait Show[Self]:
  type Out
  def show(self: Self): Out

given ShowPerson: Show[Person] with
  type Out = Int
  def show(self: Person): Int = self.name.length

class Person(val name: String)(val showW: Show[Person]):
  def show = showW.show(this)

given ShowString: Show[String] with
  type Out = Double
  def show(self: String): Double = self.length.toDouble

class Person1[S <: Show[String]](val name: String)(val showW: S):
  def show = showW.show(name)

def Test =
  val kasia = Person("Kasia")(ShowPerson)
  val _: Int = kasia.show
  val kasia1 = Person1("Kasia")(ShowString)
  val _: Double = kasia1.show
