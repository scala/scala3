class Box[T](val x: T)

class Foo(val id: String):
  def this(x: Int) = this(x.toString)

case class PersonCase(name: String, age: Int)

case class PersonCaseCurried(name: String)(val age: Int)

case class PersonCaseMutable(name: String, var age: Int)

case class PersonCaseSecondary(name: String, age: Int):
  def this(name: String) = this(name, 0)

def test: Unit =
  summon[{b: Box[Int] with 3 == b.x} =:= {b: Box[Int] with b.x == 3}]
  summon[{f: Foo with f.id == "hello"} =:= {f: Foo with "hello" == f.id}]

  // new PersonCase
  summon[{p: PersonCase with p == new PersonCase("Alice", 30)} =:= {p: PersonCase with p == new PersonCase("Alice", 30)}]
  summon[{s: String with new PersonCase("Alice", 30).name == s} =:= {s: String with s == "Alice"}]
  summon[{n: Int with new PersonCase("Alice", 30).age == n} =:= {n: Int with n == 30}]

  // PersonCase
  summon[{p: PersonCase with p == PersonCase("Alice", 30)} =:= {p: PersonCase with p == PersonCase("Alice", 30)}]
  summon[{s: String with PersonCase("Alice", 30).name == s} =:= {s: String with s == "Alice"}]
  summon[{n: Int with PersonCase("Alice", 30).age == n} =:= {n: Int with n == 30}]

  // new PersonCaseCurried
  summon[{p: PersonCaseCurried with p == new PersonCaseCurried("Alice")(30)} =:= {p: PersonCaseCurried with p == new PersonCaseCurried("Alice")(30)}]
  summon[{s: String with new PersonCaseCurried("Alice")(30).name == s} =:= {s: String with s == "Alice"}]
  summon[{n: Int with new PersonCaseCurried("Alice")(30).age == n} =:= {n: Int with n == 30}]

  // PersonCaseCurried
  summon[{p: PersonCaseCurried with p == PersonCaseCurried("Alice")(30)} =:= {p: PersonCaseCurried with p == PersonCaseCurried("Alice")(30)}]
  summon[{s: String with PersonCaseCurried("Alice")(30).name == s} =:= {s: String with s == "Alice"}]
  summon[{n: Int with PersonCaseCurried("Alice")(30).age == n} =:= {n: Int with n == 30}]

  // new PersonCaseMutable
  summon[{p: PersonCaseMutable with p == new PersonCaseMutable("Alice", 30)} =:= {p: PersonCaseMutable with p == new PersonCaseMutable("Alice", 30)}]
  summon[{s: String with new PersonCaseMutable("Alice", 30).name == s} =:= {s: String with s == "Alice"}]
  //summon[{n: Int with new PersonCaseMutable("Alice", 30).age == n} =:= {n: Int with n == 30}] // error

  // PersonCaseMutable
  summon[{p: PersonCaseMutable with p == PersonCaseMutable("Alice", 30)} =:= {p: PersonCaseMutable with p == PersonCaseMutable("Alice", 30)}]
  summon[{s: String with PersonCaseMutable("Alice", 30).name == s} =:= {s: String with s == "Alice"}]
  //summon[{n: Int with PersonCaseMutable("Alice", 30).age == n} =:= {n: Int with n == 30}] // error

  // new PersonCaseSecondary
  summon[{p: PersonCaseSecondary with p == new PersonCaseSecondary("Alice")} =:= {p: PersonCaseSecondary with p == new PersonCaseSecondary("Alice")}]
  //summon[{s: String with new PersonCaseSecondary("Alice").name == s} =:= {s: String with s == "Alice"}] // error
  //summon[{n: Int with new PersonCaseSecondary("Alice").age == n} =:= {n: Int with n == 0}] // error

  // PersonCaseSecondary
  summon[{p: PersonCaseSecondary with p == PersonCaseSecondary("Alice", 30)} =:= {p: PersonCaseSecondary with p == PersonCaseSecondary("Alice", 30)}]
  //summon[{s: String with PersonCaseSecondary("Alice", 30).name == s} =:= {s: String with s == "Alice"}] // error
  //summon[{n: Int with PersonCaseSecondary("Alice", 30).age == n} =:= {n: Int with n == 30}] // error
