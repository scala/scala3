//> using options -Xfatal-warnings
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)

object Test {

  if (true) {
    println("hi")

  println("!")  // warn: too far to the left
}
// error: too far to the left