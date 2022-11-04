object Test {
  val s = ""
  import s.{length => x, size => y} // error: size is not a member of s
  locally(x) // ok
  locally(size) // error: Not found: y
}
