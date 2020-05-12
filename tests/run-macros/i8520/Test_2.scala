trait X[+A]
trait Y[-B]
trait Z[C]

@main def Test = {
  test[X]
  test[Y]
  test[Z]
}
