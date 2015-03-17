package test

trait T {

  val x = 2

  var y = 2

  private[this] var z = 3

  private var a = 3

}
class C {

  val x = 2

  var y = 2

  private[this] var z = 3

  private var a = 3
}
