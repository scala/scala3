case class User(name: String, age: Int)
case class Person(first: String, last: String, age: Int, city: String)

object Test:
  def main(args: Array[String]): Unit =
    val myUser = User("James", 26)
    val myPerson = Person("Alice", "Smith", 30, "NYC")
    val people = Seq(myPerson, Person("Bob", "Jones", 25, "LA"))

    // 2-field case class: val patterns
    val User(name = n1) = myUser
    assert(n1 == "James")

    val User(age = a1) = myUser
    assert(a1 == 26)

    val User(name = n2, age = a2) = myUser
    assert(n2 == "James")
    assert(a2 == 26)

    val User(age = a3, name = n3) = myUser  // reversed order
    assert(n3 == "James")
    assert(a3 == 26)

    // 4-field case class: single field patterns
    val Person(first = f1) = myPerson
    assert(f1 == "Alice")

    val Person(last = l1) = myPerson
    assert(l1 == "Smith")

    val Person(age = age1) = myPerson
    assert(age1 == 30)

    val Person(city = c1) = myPerson
    assert(c1 == "NYC")

    // 4-field case class: two field patterns (various combinations)
    val Person(first = f2, last = l2) = myPerson
    assert(f2 == "Alice")
    assert(l2 == "Smith")

    val Person(first = f3, age = age2) = myPerson
    assert(f3 == "Alice")
    assert(age2 == 30)

    val Person(first = f4, city = c2) = myPerson
    assert(f4 == "Alice")
    assert(c2 == "NYC")

    val Person(last = l3, age = age3) = myPerson
    assert(l3 == "Smith")
    assert(age3 == 30)

    val Person(last = l4, city = c3) = myPerson
    assert(l4 == "Smith")
    assert(c3 == "NYC")

    val Person(age = age4, city = c4) = myPerson
    assert(age4 == 30)
    assert(c4 == "NYC")

    // 4-field case class: two fields in reversed order
    val Person(last = l5, first = f5) = myPerson
    assert(f5 == "Alice")
    assert(l5 == "Smith")

    val Person(city = c5, first = f6) = myPerson
    assert(f6 == "Alice")
    assert(c5 == "NYC")

    val Person(city = c6, age = age5) = myPerson
    assert(age5 == 30)
    assert(c6 == "NYC")

    // 4-field case class: three field patterns
    val Person(first = f7, last = l6, age = age6) = myPerson
    assert(f7 == "Alice")
    assert(l6 == "Smith")
    assert(age6 == 30)

    val Person(first = f8, last = l7, city = c7) = myPerson
    assert(f8 == "Alice")
    assert(l7 == "Smith")
    assert(c7 == "NYC")

    val Person(first = f9, age = age7, city = c8) = myPerson
    assert(f9 == "Alice")
    assert(age7 == 30)
    assert(c8 == "NYC")

    val Person(last = l8, age = age8, city = c9) = myPerson
    assert(l8 == "Smith")
    assert(age8 == 30)
    assert(c9 == "NYC")

    // 4-field case class: three fields in various orders
    val Person(age = age9, first = f10, last = l9) = myPerson
    assert(f10 == "Alice")
    assert(l9 == "Smith")
    assert(age9 == 30)

    val Person(city = c10, last = l10, first = f11) = myPerson
    assert(f11 == "Alice")
    assert(l10 == "Smith")
    assert(c10 == "NYC")

    val Person(city = c11, age = age10, first = f12) = myPerson
    assert(f12 == "Alice")
    assert(age10 == 30)
    assert(c11 == "NYC")

    // 4-field case class: all four fields
    val Person(first = f13, last = l11, age = age11, city = c12) = myPerson
    assert(f13 == "Alice")
    assert(l11 == "Smith")
    assert(age11 == 30)
    assert(c12 == "NYC")

    val Person(city = c13, age = age12, last = l12, first = f14) = myPerson  // fully reversed
    assert(f14 == "Alice")
    assert(l12 == "Smith")
    assert(age12 == 30)
    assert(c13 == "NYC")

    val Person(last = l13, city = c14, first = f15, age = age13) = myPerson  // mixed order
    assert(f15 == "Alice")
    assert(l13 == "Smith")
    assert(age13 == 30)
    assert(c14 == "NYC")

    // For-comprehension patterns with 2-field case class
    val names1 = for User(name = x) <- Seq(myUser) yield x
    assert(names1 == Seq("James"))

    val ages1 = for User(age = x) <- Seq(myUser) yield x
    assert(ages1 == Seq(26))

    val pairs1 = for User(name = n, age = a) <- Seq(myUser) yield (n, a)
    assert(pairs1 == Seq(("James", 26)))

    val pairs2 = for User(age = a, name = n) <- Seq(myUser) yield (n, a)
    assert(pairs2 == Seq(("James", 26)))

    // For-comprehension patterns with 4-field case class
    val firsts = for Person(first = x) <- people yield x
    assert(firsts == Seq("Alice", "Bob"))

    val lasts = for Person(last = x) <- people yield x
    assert(lasts == Seq("Smith", "Jones"))

    val ages2 = for Person(age = x) <- people yield x
    assert(ages2 == Seq(30, 25))

    val cities = for Person(city = x) <- people yield x
    assert(cities == Seq("NYC", "LA"))

    // For-comprehension: multiple fields
    val twoFields1 = for Person(first = f, last = l) <- people yield (f, l)
    assert(twoFields1 == Seq(("Alice", "Smith"), ("Bob", "Jones")))

    val twoFields2 = for Person(city = c, age = a) <- people yield (c, a)
    assert(twoFields2 == Seq(("NYC", 30), ("LA", 25)))

    val twoFields3 = for Person(last = l, first = f) <- people yield (f, l)  // reversed
    assert(twoFields3 == Seq(("Alice", "Smith"), ("Bob", "Jones")))

    val threeFields1 = for Person(first = f, last = l, age = a) <- people yield (f, l, a)
    assert(threeFields1 == Seq(("Alice", "Smith", 30), ("Bob", "Jones", 25)))

    val threeFields2 = for Person(city = c, first = f, age = a) <- people yield (f, a, c)
    assert(threeFields2 == Seq(("Alice", 30, "NYC"), ("Bob", 25, "LA")))

    val allFields1 = for Person(first = f, last = l, age = a, city = c) <- people yield (f, l, a, c)
    assert(allFields1 == Seq(("Alice", "Smith", 30, "NYC"), ("Bob", "Jones", 25, "LA")))

    val allFields2 = for Person(city = c, age = a, last = l, first = f) <- people yield (f, l, a, c)
    assert(allFields2 == Seq(("Alice", "Smith", 30, "NYC"), ("Bob", "Jones", 25, "LA")))

    // Match expressions (these should continue to work)
    val matchResult1 = myUser match
      case User(name = x) => x
    assert(matchResult1 == "James")

    val matchResult2 = myUser match
      case User(age = x, name = n) => (n, x)
    assert(matchResult2 == ("James", 26))

    val matchResult3 = myPerson match
      case Person(first = f, city = c) => (f, c)
    assert(matchResult3 == ("Alice", "NYC"))

    val matchResult4 = myPerson match
      case Person(city = c, age = a, last = l, first = f) => (f, l, a, c)
    assert(matchResult4 == ("Alice", "Smith", 30, "NYC"))
