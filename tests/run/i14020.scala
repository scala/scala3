class A:
  def greeting(name: String = "you") = s"Hello $name"

class B:
  val a = A()
  export a.*

class C:
  val a = A()
  export a.greeting

@main def Test =
  val b = B()

  println(b.a.greeting())      // works
  println(b.greeting("John"))  // works
  println(b.greeting())        // nope !

  val c = C()

  println(c.a.greeting())      // works
  println(c.greeting("John"))  // works
  println(c.greeting())        // nope !

  val w = Wolf()
  import w.given

  println(summon[String]) // error: I found: w.bark(/* missing */summon[String])

class Dog:
  given bark(using msg: String = "Woof!"): String = s"bark: $msg"

class Wolf:
  private val dog = Dog()
  export dog.given // needs to be `export dog.{given, *}` to export the default arguments
