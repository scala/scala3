//> using options -Werror

class Test {
  type MyCombo = Int | Unit
  val z: MyCombo = 10
   z match
    case i: Int => ???
    case _ => ???
}
