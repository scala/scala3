// test for exception
object assignments {

  var a = Array(1, 2, 3)
  var i = 0
  a(i) = a(i) * 2
  a(i + 1) += 1

  class C {
    var myX = 0
    def x = myX
    def x_=(x: Int) = myX = x

    x = x + 1
    x *= 2
  }

  var c = new C
  c.x =c.x + 1
  c.x = c.x * 2

  val cc = c
  import cc.*
  x = x + 1
  x *= 2
}
