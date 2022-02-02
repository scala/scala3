final class MyIsInstanceOfClass(o: MyIsInstanceOfClass) {
  val other: MyIsInstanceOfClass = {
    if (!o.isInstanceOf[Object]) new MyIsInstanceOfClass(this) // o is cold, but isInstanceOf is always valid
    else o
  }
}

final class MyAsInstanceOfClass(o: MyAsInstanceOfClass) {
  val other: MyAsInstanceOfClass = {
    if (o.asInstanceOf[Object] ne null) o // o is cold, but ne and AsInstanceOf is always valid
    else new MyAsInstanceOfClass(this)
  }
}

final class MyAsInstanceOfFieldClass(o: MyAsInstanceOfFieldClass) {
  val oRef = o
  val other: MyAsInstanceOfFieldClass = {
    if (this.asInstanceOf[MyAsInstanceOfFieldClass].oRef ne null) oRef // o is cold, but ne and AsInstanceOf is always valid
    else new MyAsInstanceOfFieldClass(this)
  }
}

