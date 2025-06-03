package foo.bar

/* This test checks whether InnerClasses and EnclosingMethod sections in generated class files are correct
 * for different possibilities of nesting of classes in other classes, objects and methods (the attributes are accessed via java reflection).
 * The checked cases are:
 * 1) a top level class/object
 * 2) a class/object in a top level class/object
 * 3) a class in another class/object in a top level class/object
 * 4) a class in a method in another class/object in top level class/object
 * 5) a class/object in a method in a top level class/object
 * 6) a class in a method in another method in a top level class/object
 * 7) a class in another class in a method in a top level class/object
 * Objects with and without a companion class need to be treated as separate cases
 * to ensure this works uniformly whether a mirror class is generated or not.
 * For completeness classes without a companion object were added as well.
 *
 * Names of nested definitions are derived from the name of their enclosing definition by appending a letter following the scheme below:
 * A - a class without a companion object
 * B - an object without a companion class
 * C - a class with its companion object
 * D - a method
 * Additionally a number may be added to avoid clashes between definitions from classes and their companion objects
 * (1 - defined in the companion class; 2 - defined in the companion object),
 * e.g. ACD2 - a method inside the companion object of a class inside a top level class
 *
 * Self-standing references to objects and constructor calls serve the purpose
 * of enforcing initialization and running the checks.
 */

object Checks extends checks.Checks {
  val expectedTopLevelChecksCount = 4
  val expectedMemberChecksCount = 4 * 8
  val expectedLocalChecksCount = 4 * 6

  def run() =
    new A
    B
    new C
    C

    verifyChecksCounts()
}

import Checks._


class A { topLevel =>
  checkTopLevel(this)

  class AA { nestedOnce =>
    checkMember(this, topLevel)

    class AAA { checkMember(this, nestedOnce) }
    new AAA

    def AAD(): Unit = {
      class AADA { checkLocal(this, nestedOnce) }
      new AADA
    }
    AAD()
  }
  new AA

  object AB {
    val nestedOnce = this // self alias cannot be used uniformly here: https://github.com/scala/scala3/issues/11648

    checkMember(this, topLevel)

    class ABA { checkMember(this, nestedOnce) }
    new ABA

    def ABD(): Unit = {
      class ABDA { checkLocal(this, nestedOnce) }
      new ABDA
    }
    ABD()
  }
  AB

  class AC { nestedOnce =>
    checkMember(this, topLevel)

    class ACA1 { checkMember(this, nestedOnce) }
    new ACA1

    def ACD(): Unit = {
      class ACDA { checkLocal(this, nestedOnce) }
      new ACDA
    }
    ACD()
  }
  new AC

  object AC {
    val nestedOnce = this

    checkMember(this, topLevel)

    class ACA2 { checkMember(this, nestedOnce) }
    new ACA2

    def ACD(): Unit = {
      class ACDA { checkLocal(this, nestedOnce) }
      new ACDA
    }
    ACD()
  }
  AC

  def AD(): Unit = {
    class ADA { nestedTwice =>
      checkLocal(this, topLevel)

      class ADAA { checkMember(this, nestedTwice) }
    }
    new ADA

    def ADD() = {
      class ADDA { checkLocal(this, topLevel) }
      new ADDA
    }
    ADD()
  }
  AD()
}


object B { topLevel =>
  checkTopLevel(this)

  class BA { nestedOnce =>
    checkMember(this, topLevel)

    class BAA { checkMember(this, nestedOnce) }
    new BAA

    def BAD(): Unit = {
      class BADA { checkLocal(this, nestedOnce) }
      new BADA
    }
    BAD()
  }
  new BA

  object BB { nestedOnce =>
    checkMember(this, topLevel)

    class BBA { checkMember(this, nestedOnce) }
    new BBA

    def BBD(): Unit = {
      class BBDA { checkLocal(this, nestedOnce) }
      new BBDA
    }
    BBD()
  }
  BB

  class BC { nestedOnce =>
    checkMember(this, topLevel)

    class BCA1 { checkMember(this, nestedOnce) }
    new BCA1

    def BCD(): Unit = {
      class BCDA { checkLocal(this, nestedOnce) }
      new BCDA
    }
    BCD()
  }
  new BC

  object BC { nestedOnce =>
    checkMember(this, topLevel)

    class BCA2 { checkMember(this, nestedOnce) }
    new BCA2

    def BCD(): Unit = {
      class BCDA { checkLocal(this, nestedOnce) }
      new BCDA
    }
    BCD()
  }
  BC

  def BD(): Unit = {
    class BDA { nestedTwice =>
      checkLocal(this, topLevel)

      class BDAA { checkMember(this, nestedTwice) }
    }
    new BDA

    def BDD() = {
      class BDDA { checkLocal(this, topLevel) }
      new BDDA
    }
    BDD()
  }
  BD()
}


class C { topLevel =>
  checkTopLevel(this)

  class CA1 { nestedOnce =>
    checkMember(this, topLevel)

    class CA1A { checkMember(this, nestedOnce) }
    new CA1A

    def CA1D(): Unit = {
      class CA1DA { checkLocal(this, nestedOnce) }
      new CA1DA
    }
    CA1D()
  }
  new CA1

  object CB1 {
    val nestedOnce = this

    checkMember(this, topLevel)

    class CB1A { checkMember(this, nestedOnce) }
    new CB1A

    def CB1D(): Unit = {
      class CB1DA { checkLocal(this, nestedOnce) }
      new CB1DA
    }
    CB1D()
  }
  CB1

  class CC1 { nestedOnce =>
    checkMember(this, topLevel)

    class CC1A1 { checkMember(this, nestedOnce) }
    new CC1A1

    def CC1D(): Unit = {
      class CC1DA { checkLocal(this, nestedOnce) }
      new CC1DA
    }
    CC1D()
  }
  new CC1

  object CC1 {
    val nestedOnce = this

    checkMember(this, topLevel)

    class CC1A2 { checkMember(this, nestedOnce) }
    new CC1A2

    def CC1D(): Unit = {
      class CC1DA { checkLocal(this, nestedOnce) }
      new CC1DA
    }
    CC1D()
  }
  CC1

  def CD1(): Unit = {
    class CD1A { nestedTwice =>
      checkLocal(this, topLevel)

      class CD1AA { checkMember(this, nestedTwice) }
    }
    new CD1A

    def CD1D() = {
      class CD1DA { checkLocal(this, topLevel) }
      new CD1DA
    }
    CD1D()
  }
  CD1()
}


object C { topLevel =>
  checkTopLevel(this)

  class CA2 { nestedOnce =>
    checkMember(this, topLevel)

    class CA2A { checkMember(this, nestedOnce) }
    new CA2A

    def CA2D(): Unit = {
      class CA2DA { checkLocal(this, nestedOnce) }
      new CA2DA
    }
    CA2D()
  }
  new CA2

  object CB2 { nestedOnce =>
    checkMember(this, topLevel)

    class CB2A { checkMember(this, nestedOnce) }
    new CB2A

    def CB2D(): Unit = {
      class CB2DA { checkLocal(this, nestedOnce) }
      new CB2DA
    }
    CB2D()
  }
  CB2

  class CC2 { nestedOnce =>
    checkMember(this, topLevel)

    class CC2A1 { checkMember(this, nestedOnce) }
    new CC2A1

    def CC2D(): Unit = {
      class CC2DA { checkLocal(this, nestedOnce) }
      new CC2DA
    }
    CC2D()
  }
  new CC2

  object CC2 { nestedOnce =>
    checkMember(this, topLevel)

    class CC2A2 { checkMember(this, nestedOnce) }
    new CC2A2

    def CC2D(): Unit = {
      class CC2DA { checkLocal(this, nestedOnce) }
      new CC2DA
    }
    CC2D()
  }
  CC2

  def CD2(): Unit = {
    class CD2A { nestedTwice =>
      checkLocal(this, topLevel)

      class CD2AA { checkMember(this, nestedTwice) }
    }
    new CD2A

    def CD2D() = {
      class CD2DA { checkLocal(this, topLevel) }
      new CD2DA
    }
    CD2D()
  }
  CD2()
}
