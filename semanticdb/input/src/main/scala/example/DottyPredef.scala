package example

class PredefsDotty {
  locally {
    val x: Int => Int = _ => ???
  }
  assert(true)
  assert(false, "bonjour")
}