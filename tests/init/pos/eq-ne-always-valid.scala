final class MyNeClass(o: MyNeClass) {
  val other: MyNeClass = {
    if (o ne null) o // o is cold, but ne is always valid
    else new MyNeClass(this)
  }
}

final class MyEqClass(o: MyEqClass) {
  val other: MyEqClass = {
    if (o eq null) new MyEqClass(this) // o is cold, but eq is always valid
    else o
  }
}
