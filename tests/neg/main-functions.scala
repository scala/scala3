class TC
object Test1 {
  @main def f(x: TC) = () // error: no implicit argument of type util.FromString[TC] was found
}
