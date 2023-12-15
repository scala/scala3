final class MyAsInstanceOfClass(o: MyAsInstanceOfClass) {
  val other: MyAsInstanceOfClass = {
    if (o.asInstanceOf[MyAsInstanceOfClass].oRef ne null) o
    else new MyAsInstanceOfClass(this)  // warn
  }
  val oRef = o
}
