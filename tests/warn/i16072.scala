//> using options -deprecation

object Hello_: // warn colon in name without backticks because the body is empty
  def x = 1 // warn too far right

object Goodbye_: : // nowarn if non-empty body without nit-picking about backticks
  def x = 2

object `Byte_`:
  def x = 3

object :: : // warn deprecated colon without backticks for operator name
  def x = 42

object ::: // nowarn

object Braces_: { // nowarn because body is non-empty with an EmptyTree
}

class Uhoh_: // warn
  def y = 1 // warn

@main def hello =
  println(Byte_)
  println(Hello_:) // apparently user did forget a colon, see https://youforgotapercentagesignoracolon.com/
  println(x)
