package toString:
  trait ToString[A] {
    def print(a: A): Unit
  }

import toString._

package A {
  case class AA(text: String)
  given ToString[AA] = aa => println(aa.text)

  opaque type AB = String
  given ToString[AB] = ab => println(ab)

  opaque type AC = String
  given ToString[AC] with {
    def print(ac: AC): Unit = println(ac)
  }
}

package B {
  case class BA(text: String)
  object BA {
    given ToString[BA] = ba => println(ba.text)
  }

  opaque type BB = String
  object BB {
    given ToString[BB] = bb => println(bb)
  }

  opaque type BC = String
  object BC {
    given ToString[BC] with {
      def print(bc: BC): Unit = println(bc)
    }
  }
}

object Test {
  val AA = summon[ToString[A.AA]] // error
  val AB = summon[ToString[A.AB]] // error, used to compile
  val AC = summon[ToString[A.AC]] // error

  val BA = summon[ToString[B.BA]]
  val BB = summon[ToString[B.BB]]
  val BC = summon[ToString[B.BC]]
}
