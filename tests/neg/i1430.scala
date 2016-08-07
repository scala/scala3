class Inv[T](x: T)
object Test {

  val x: List[String] = List(1) // error: found Int(1), expected: String

  val y: List[List[String]] = List(List(1)) // error: found Int(1), expected: String

  val z: (List[String], List[Int]) = (List(1), List("a")) // error: found Int(1), expected: String // error: found String(a), expected: Int

  val a: Inv[String] = new Inv(new Inv(1)) // error: found Inv[T], expected: String ... where  T  ...

  val b: Inv[String] = new Inv(1) // error: found Int, expected: String

  abstract class C {
    type T
    val x: T
    val s: Unit = {
      type T = String
      var y: T = x  // error
      locally { def f() = {
          new {
            type T = Int
            val z: T = y // error
          }
          Nil map { _ =>
            type T = Int
            val z: T = y // error
          }
        }
        f()
      }
    }
  }
}
