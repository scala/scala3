package foo.bar

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

/* The checked cases are:
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
 * Self-standing references to objects and constructor calls serve the purpose
 * of enforcing initialization and running the checks.
 */

class A { topLevel =>
  checkTopLevel[this.type]

  class AA { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class AAA { checkMember[this.type, nestedOnce.type] }
    new AAA

    def AAD(): Unit = {
      class AADA { checkLocal[this.type, nestedOnce.type] }
      new AADA
    }
    AAD()
  }
  new AA

  object AB { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class ABA { checkMember[this.type, nestedOnce.type] }
    new ABA

    def ABD(): Unit = {
      class ABDA { checkLocal[this.type, nestedOnce.type] }
      new ABDA
    }
    ABD()
  }
  AB

  class AC { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class ACA1 { checkMember[this.type, nestedOnce.type] }
    new ACA1

    def ACD(): Unit = {
      class ACDA { checkLocal[this.type, nestedOnce.type] }
      new ACDA
    }
    ACD()
  }
  new AC

  object AC { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class ACA2 { checkMember[this.type, nestedOnce.type] }
    new ACA2

    def ACD(): Unit = {
      class ACDA { checkLocal[this.type, nestedOnce.type] }
      new ACDA
    }
    ACD()
  }
  AC

  def AD(): Unit = {
    class ADA { nestedTwice =>
      checkLocal[this.type, topLevel.type]

      class ADAA { checkMember[this.type, nestedTwice.type] }
    }
    new ADA

    def ADD() = {
      class ADDA { checkLocal[this.type, topLevel.type] }
      new ADDA
    }
    ADD()
  }
  AD()
}


object B { topLevel =>
  checkTopLevel[this.type]

  class BA { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class BAA { checkMember[this.type, nestedOnce.type] }
    new BAA

    def BAD(): Unit = {
      class BADA { checkLocal[this.type, nestedOnce.type] }
      new BADA
    }
    BAD()
  }
  new BA

  object BB { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class BBA { checkMember[this.type, nestedOnce.type] }
    new BBA

    def BBD(): Unit = {
      class BBDA { checkLocal[this.type, nestedOnce.type] }
      new BBDA
    }
    BBD()
  }
  BB

  class BC { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class BCA1 { checkMember[this.type, nestedOnce.type] }
    new BCA1

    def BCD(): Unit = {
      class BCDA { checkLocal[this.type, nestedOnce.type] }
      new BCDA
    }
    BCD()
  }
  new BC

  object BC { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class BCA2 { checkMember[this.type, nestedOnce.type] }
    new BCA2

    def BCD(): Unit = {
      class BCDA { checkLocal[this.type, nestedOnce.type] }
      new BCDA
    }
    BCD()
  }
  BC

  def BD(): Unit = {
    class BDA { nestedTwice =>
      checkLocal[this.type, topLevel.type]

      class BDAA { checkMember[this.type, nestedTwice.type] }
    }
    new BDA

    def BDD() = {
      class BDDA { checkLocal[this.type, topLevel.type] }
      new BDDA
    }
    BDD()
  }
  BD()
}


class C { topLevel =>
  checkTopLevel[this.type]

  class CA1 { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class CA1A { checkMember[this.type, nestedOnce.type] }
    new CA1A

    def CA1D(): Unit = {
      class CA1DA { checkLocal[this.type, nestedOnce.type] }
      new CA1DA
    }
    CA1D()
  }
  new CA1

  object CB1 { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class CB1A { checkMember[this.type, nestedOnce.type] }
    new CB1A

    def CB1D(): Unit = {
      class CB1DA { checkLocal[this.type, nestedOnce.type] }
      new CB1DA
    }
    CB1D()
  }
  CB1

  class CC1 { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class CC1A1 { checkMember[this.type, nestedOnce.type] }
    new CC1A1

    def CC1D(): Unit = {
      class CC1DA { checkLocal[this.type, nestedOnce.type] }
      new CC1DA
    }
    CC1D()
  }
  new CC1

  object CC1 { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class CC1A2 { checkMember[this.type, nestedOnce.type] }
    new CC1A2

    def CC1D(): Unit = {
      class CC1DA { checkLocal[this.type, nestedOnce.type] }
      new CC1DA
    }
    CC1D()
  }
  CC1

  def CD1(): Unit = {
    class CD1A { nestedTwice =>
      checkLocal[this.type, topLevel.type]

      class CD1AA { checkMember[this.type, nestedTwice.type] }
    }
    new CD1A

    def CD1D() = {
      class CD1DA { checkLocal[this.type, topLevel.type] }
      new CD1DA
    }
    CD1D()
  }
  CD1()
}


object C { topLevel =>
  checkTopLevel[this.type]

  class CA2 { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class CA2A { checkMember[this.type, nestedOnce.type] }
    new CA2A

    def CA2D(): Unit = {
      class CA2DA { checkLocal[this.type, nestedOnce.type] }
      new CA2DA
    }
    CA2D()
  }
  new CA2

  object CB2 { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class CB2A { checkMember[this.type, nestedOnce.type] }
    new CB2A

    def CB2D(): Unit = {
      class CB2DA { checkLocal[this.type, nestedOnce.type] }
      new CB2DA
    }
    CB2D()
  }
  CB2

  class CC2 { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class CC2A1 { checkMember[this.type, nestedOnce.type] }
    new CC2A1

    def CC2D(): Unit = {
      class CC2DA { checkLocal[this.type, nestedOnce.type] }
      new CC2DA
    }
    CC2D()
  }
  new CC2

  object CC2 { nestedOnce =>
    checkMember[this.type, topLevel.type]

    class CC2A2 { checkMember[this.type, nestedOnce.type] }
    new CC2A2

    def CC2D(): Unit = {
      class CC2DA { checkLocal[this.type, nestedOnce.type] }
      new CC2DA
    }
    CC2D()
  }
  CC2

  def CD2(): Unit = {
    class CD2A { nestedTwice =>
      checkLocal[this.type, topLevel.type]

      class CD2AA { checkMember[this.type, nestedTwice.type] }
    }
    new CD2A

    def CD2D() = {
      class CD2DA { checkLocal[this.type, topLevel.type] }
      new CD2DA
    }
    CD2D()
  }
  CD2()
}
