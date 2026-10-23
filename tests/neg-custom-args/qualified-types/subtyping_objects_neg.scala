class Box[T](val x: T)

class BoxMutable[T](var x: T)

class Foo(val id: String):
  def this(x: Int) = this(x.toString)

class Person(val name: String, val age: Int)

class PersonCurried(val name: String)(val age: Int)

class PersonMutable(val name: String, var age: Int)

case class PersonCaseMutable(name: String, var age: Int)

case class PersonCaseSecondary(name: String, age: Int):
  def this(name: String) = this(name, 0)

case class PersonCaseEqualsOverriden(name: String, age: Int):
  override def equals(that: Object): Boolean = this eq that

def test: Unit =
  summon[{b: Box[Int] with b == Box(1)} =:= {b: Box[Int] with b == Box(1)}] // error // error

  summon[{b: BoxMutable[Int] with b == BoxMutable(1)} =:= {b: BoxMutable[Int] with b == BoxMutable(1)}] // error // error
  // TODO(mbovel): restrict selection to stable members
  //summon[{b: BoxMutable[Int] with b.x == 3} =:= {b: BoxMutable[Int] with b.x == 3}]

  summon[{f: Foo with f == Foo("hello")} =:= {f: Foo with f == Foo("hello")}] // error // error
  summon[{f: Foo with f == Foo(1)} =:= {f: Foo with f == Foo(1)}] // error // error
  summon[{s: String with Foo("hello").id == s} =:= {s: String with s == "hello"}] // error

  summon[{p: Person with p == Person("Alice", 30)} =:= {p: Person with p == Person("Alice", 30)}] // error // error
  summon[{s: String with Person("Alice", 30).name == s} =:= {s: String with s == "Alice"}] // error
  summon[{n: Int with Person("Alice", 30).age == n} =:= {n: Int with n == 30}] // error

  summon[{p: PersonCurried with p == PersonCurried("Alice")(30)} =:= {p: PersonCurried with p == PersonCurried("Alice")(30)}] // error // error
  summon[{s: String with PersonCurried("Alice")(30).name == s} =:= {s: String with s == "Alice"}] // error
  summon[{n: Int with PersonCurried("Alice")(30).age == n} =:= {n: Int with n == 30}] // error

  summon[{p: PersonMutable with p == PersonMutable("Alice", 30)} =:= {p: PersonMutable with p == PersonMutable("Alice", 30)}] // error // error
  summon[{s: String with PersonMutable("Alice", 30).name == s} =:= {s: String with s == "Alice"}] // error
  summon[{n: Int with PersonMutable("Alice", 30).age == n} =:= {n: Int with n == 30}] // error

  summon[{n: Int with PersonCaseMutable("Alice", 30).age == n} =:= {n: Int with n == 30}] // error

  summon[{s: String with new PersonCaseSecondary("Alice").name == s} =:= {s: String with s == "Alice"}] // error
  summon[{n: Int with new PersonCaseSecondary("Alice").age == n} =:= {n: Int with n == 0}] // error

  summon[{p: PersonCaseEqualsOverriden with PersonCaseEqualsOverriden("Alice", 30) == p} =:= {p: PersonCaseEqualsOverriden with p == PersonCaseEqualsOverriden("Alice", 30)}] // error // error
  summon[{s: String with PersonCaseEqualsOverriden("Alice", 30).name == s} =:= {s: String with s == "Alice"}] // error
  summon[{n: Int with PersonCaseEqualsOverriden("Alice", 30).age == n} =:= {n: Int with n == 30}] // error
