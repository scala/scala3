//> using options -deprecation

@deprecated("is deprecated", "n/a")
class Thing(val value: Int)

object Main {

  def doo(): Option[Thing] = // warn
    Some(new Thing(1)) // warn
  def wop(x: => Option[Thing]) = println(x) // warn

  doo().map(t => println(t))
  doo().map((t: Thing) => println(t)) // warn
  doo().map(println)
  doo().foreach(println)
  for (x <- doo()) println(x)

  val thing = new Thing(42) // warn
  println(thing)

  val something = Some(thing)
  wop(something)
}