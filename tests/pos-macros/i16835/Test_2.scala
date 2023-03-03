import scala.deriving.*

object usage {
  final case class Person(name: String, age: Int, otherNames: List[String], p2: Person2)

  final case class Person2(name: String, age: Int, otherNames: List[String])

  locally {
    import Derivation.deriveFullyConstrucedByMacro
    // works for case classes without other nested case classes inside
    summon[Show[Person2]]

    // also derives instances with nested case classes
    summon[Show[Person]]
  }

  locally {
    import Derivation.derivePartiallyConstructedByMacro

    // works for case classes without other nested case classes inside
    summon[Show[Person2]]

    // fails for case classes with other nested case classes inside,
    // note how that error is not a `NonMatching', `Diverging` or `Ambiguous` implicit search error but something else
    /*
    catch all: given instance deriveWithConstructionOutsideMacro in object Derivation does not match type io.github.arainko.ducktape.issue_repros.Show[Person2]
    */
    summon[Show[Person]]
  }
}