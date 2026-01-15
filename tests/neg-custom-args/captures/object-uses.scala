package test

class File

class A {
  val f: File^ = File()
  val g: File^ = File()

  object B uses f:
    def show =
      Console.println(f.toString)
      Console.println(g.toString) // error

  val b = B
  val _: Object = b // error

}