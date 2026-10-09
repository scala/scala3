//> using option -Wunused:privates

class A {
  private given Option[Int] = Some(2) // warn

  private given List[Int] = List(3) // warn

  private given given_Int: Int = 42 // warn

  private val given_String: String = "hello, world" // warn underlining possibly poorly chosen name
}
