object Test {

  transparent def swap[T](x: T, x_= : => T => Unit, y: T, y_= : => T => Unit) = {
    x_=(y)
    y_=(x)
  }

  transparent def f(x: Int => Unit) = x

  def main(args: Array[String]) = {
    var x = 1
    var y = 2
    transparent def setX(z: Int) = x = z
    transparent def setY(z: Int) = y = z
    swap(x, setX, y, setY)
    assert(x == 2 && y == 1)

    swap(x, x = _, y, y = _)
    assert(x == 1 && y == 2)


    val z = f(setX)  // tests case where inline arg is not applied
  }
}
