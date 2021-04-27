case class Name(first: String, last: String)
case class Age(age: Int)
case class Person(name: Name, age: Age)
case class Config(name: String, age: Int)



object Imperative {
  import Configs.*
  import Exceptions.*

  // Algebraic Effects

  def readName: Possibly[Configured[Name]] = {
    val parts = config.name.split(" ")
    require(parts.length >= 2)
    Name(parts(0), parts.tail.mkString(" "))
  }

  def readAge: Configured[Possibly[Age]] = {
    val age = config.age
    require(1 <= age && age <= 150)
    Age(age)
  }

  def readPerson: Configured[Option[Person]] =
    attempt(
      Some(Person(readName, readAge))
    ).onError(None)

  def main(args: Array[String]) = {
    println(readPerson(using Config("John Doe", 20)))
    println(readPerson(using Config("Incognito", 99)))
  }
}

object Configs {
  type Configured[T] = Config ?=> T
  def config: Configured[Config] = implicitly[Config]
}

object Exceptions {

  private class E extends Exception

  class CanThrow private[Exceptions] () {
    private[Exceptions] def throwE() = throw new E
  }

  type Possibly[T] = CanThrow ?=> T

  def require(p: Boolean)(implicit ct: CanThrow): Unit =
    if (!p) ct.throwE()

  def attempt[T](op: Possibly[T]) = new OnError(op)

  class OnError[T](op: Possibly[T]) {
    def onError(fallback: => T): T =
      try op(using new CanThrow)
      catch { case ex: E => fallback }
  }
}

object Test extends App {
  import Configs.*
  import Exceptions.*

  def readName: Configured[Possibly[Name]] = {
    val parts = config.name.split(" ")
    require(parts.length >= 2)
    Name(parts(0), parts.tail.mkString(" "))
  }

  def readAge: Possibly[Configured[Age]] = {
    val age = config.age
    require(1 <= age && age <= 150)
    Age(age)
  }

  def readPerson: Configured[Option[Person]] =
    attempt(
      Some(Person(readName, readAge))
    ).onError(None)

  val config1 = Config("John Doe", 20)
  val config2 = Config("Incognito", 99)

  println(readPerson(using config1))
  println(readPerson(using config2))
}

object OptionTest extends App {

  def readName(config: Config): Option[Name] = {
    val parts = config.name.split(" ")
    if (parts.length >= 2) Some(Name(parts(0), parts.tail.mkString(" ")))
    else None
  }

  def readAge(config: Config): Option[Age] = {
    val age = config.age
    if (1 <= age && age <= 150) Some(Age(age)) else None
  }

  def readPerson(config: Config): Option[Person] =
    for {
      name <- readName(config)
      age <- readAge(config)
    }
    yield Person(name, age)

  val config1 = Config("John Doe", 20)
  val config2 = Config("Incognito", 99)

  println(readPerson(config1))
  println(readPerson(config2))
}

object FancyStuff {
  import Configs.*
  import Exceptions.*
  import Test.*

  type PC[T] = Possibly[Configured[T]]

  val names: PC[List[Name]] = readName :: Nil
  val firstNames: PC[List[String]] = names.map(_.first)
  val longest: PC[String] = firstNames.maxBy(_.length)

  val xs: List[PC[String]] = List(longest)
  val ys: PC[List[String]] = xs.map(x => x)
}