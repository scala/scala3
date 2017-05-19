object Test1 with

  val xs = List(1, 2, 3)

  xs.map with
    x => x + 3

object Test with

  val xs = List(1, 2, 3)

  xs.map with x =>
       x + 2
    .filter with
       x => x % 2 == 0
    .foldLeft(0) with
       _ + _

  xs.map with x =>
      x + 2
    .filter with x =>
      x % 2 == 0
    .foldLeft(0) with
      _ + _

// end Test

package p with

  object o with

    class C extends Object
               with Serializable with

      val x = new C with
          def y = 3

      val result =
        if x == x then
          println("yes")
          true
        else
          println("no")
          false
