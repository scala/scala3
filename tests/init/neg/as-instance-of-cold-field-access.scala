final class MyAsInstanceOfClass(o: MyAsInstanceOfClass) {
  val other: MyAsInstanceOfClass = {
    if (o.asInstanceOf[MyAsInstanceOfClass].oRef ne null) o // error
    else new MyAsInstanceOfClass(this)
  }
  val oRef = o
}
