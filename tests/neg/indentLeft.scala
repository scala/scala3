//> using options -Xfatal-warnings
// nopos-error: No warnings can be incurred under -Werror.

object Test {

  if (true) {
    println("hi")

  println("!")  // warn: too far to the left
}
// error: too far to the left