@main def Test: Unit =

  ({println ("Hello"); 4}).isInstanceOf[{ y : Int with y > 0}]

  class Person(val name: String, var age: Int)
    def incAge(p: Person): Int =
        p.age += 1
        p.age

  val p = new Person("Alice", 64)

  if incAge(p).isInstanceOf[{v:Int with v == 65}] then println("succeed")
  println(p.age)

  ({println ("Hello"); 4}).isInstanceOf[Int & { y : Int with y > 0}]
