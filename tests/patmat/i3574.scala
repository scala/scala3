object PathVariantDefns {
  sealed trait AtomBase {
    sealed trait Atom
    case class Zero(value: String) extends Atom
  }

  trait Atom1 extends AtomBase {
    case class One(value: String) extends Atom
  }

  trait Atom2 extends AtomBase {
    case class Two(value: String) extends Atom
  }

  object Atoms01 extends AtomBase with Atom1 {
    def f = {
      val a: Atom = null
      a match {
        case Zero(x) => ???
        case One(x)  => ???
        // match may not be exhaustive.
        // It would fail on: PathVariantDefns.Atom2 & PathVariantDefns.Atoms01.type(PathVariantDefns.Atoms01).Two(_)
      }
    }
  }

  object Atoms02 extends AtomBase with Atom2 {
    def f = {
      val a: Atom = null
      a match {
        case Zero(x) => ???
        case Two(x)  => ???
        // match may not be exhaustive.
        // It would fail on: PathVariantDefns.Atom1 & PathVariantDefns.Atoms02.type(PathVariantDefns.Atoms02).One(_)
      }
    }
  }
}
