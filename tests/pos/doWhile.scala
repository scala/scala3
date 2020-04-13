import language.`3.0-migration`
class Test {
  do {
    val x = 1
    println(x)
  } while {
    val x = "a"
    println(x)
    true
  }

  do {
    val x = 1
  } while {
    val x = "a"
    true
  }
}
class Test2 {
  val x: Int = 3
  do {
    val x = ""
  } while (x == 2)

  do (x == 3)
  while {
    val x = "a"
    true
  }

}
