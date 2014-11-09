trait B extends Object {

  val z: Int

}

trait T extends B {

  var x = 2

  private var xp = 2

  val y = 3

  private val yp = 3

  val z = 4

  x = 4

  xp = 4

}

class C extends T
