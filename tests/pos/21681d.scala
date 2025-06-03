def test1() =
  class Person:
    def age: Int = ???
    def age_=(x: Int): Unit = ???

  val person = Person()
  
  (person.age = 29) // no warn (interpreted as `person.age_=(29)`)

def test2() =
  class Person:
    var age: Int = 28

  val person = Person()
  
  (person.age = 29) // no warn (interpreted as `person.age_=(29)`)
