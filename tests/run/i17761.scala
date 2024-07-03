class MyRecord extends Selectable:
  def applyDynamic(name: String, paramClasses: Class[_]*)(args: Any*): Any = {
    println(name)
    println(paramClasses)
    println(args)
    ()
  }

class MyRecordTransparent extends Selectable:
  inline transparent def applyDynamic(name: String, paramClasses: Class[_]*)(args: Any*): Any = {
    println(name)
    println(paramClasses)
    println(args)
    ()
  }

type Person = MyRecord {
  def test(a: String, b: Int): Unit
}


type PersonTransparent = MyRecordTransparent {
  def test(a: String, b: Int): Unit
}

val person = MyRecord().asInstanceOf[Person]
val personTransparent = MyRecordTransparent().asInstanceOf[PersonTransparent]

@main def Test: Unit =
  println("Normal")
  person.test("test", 42)
  println("Transparent")
  personTransparent.test("test", 42)