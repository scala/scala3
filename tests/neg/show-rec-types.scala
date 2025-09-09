trait C {
  type T
  val x: Any
  val y: Any
}

val c: C {
  val x: C {
    val x: this.T
  }
  val y: this.T
} = ???
val _: Int = c // error


