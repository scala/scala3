import scala.language.experimental.modularity

tracked trait Show[Self]:
  type Out
  def show(self: Self): Out

given ShowPerson: Show[Person] with
  type Out = Int
  def show(self: Person): Int = self.name.length

class Person(val name: String)(val showW: Show[Person]):
  def show = showW.show(this)

def Test =
  val kasia = Person("Kasia")(ShowPerson)
  val _: Int = kasia.show
