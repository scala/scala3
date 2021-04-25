import scala.compiletime.error

object BadFilters:
  inline def withFilter(f: Int => Boolean): BadFilters.type = error("Unexpected withFilter call")
  def foreach(f: Int => Unit): Unit = f(42)

@main def Test =
  for
    x: Int <- BadFilters
  do println(x)
  for
    given Int <- BadFilters
  do println(summon[Int])
