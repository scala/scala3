// Compile with -rewrite -pascal-style
// Then compile again with -rewrite c-style
// The resulting file is the same as the original one, except for better formatting
object Test {

  val xs = List(1, 2, 3)

  for(x <- xs)yield x * 2

  for(x <- xs)
  yield x * 2

  for{ x <- xs; y <- xs }yield x * y

  for{
    x <- xs
    y <- xs
  }yield x * y

  if(xs == Nil)println("yes")

  if(xs == Nil)
    println("yes")
}