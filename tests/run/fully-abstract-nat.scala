import scala.reflect.AbstractTypeTag

object Test {
  def main(args: Array[String]): Unit = {
    println("CaseClassImplementation")
    testInterface(CaseClassImplementation)

    println()

    println("IntImplementation")
    testInterface(IntImplementation)

    println()

    println("UnboundedIntImplementation")
    testInterface(UnboundedIntImplementation)

    println()

    {
      import UnboundedIntImplementation._
      val large = (BigInt(1) << 100).asInstanceOf[Succ]
      large match {
        case Zero() => println("test fail")
        case s @ Succ(pred) =>
          println("test OK")
          println(s"Succ(${pred.pred}) = $s")
      }
    }
  }

  def testInterface(numbers: Numbers): Unit = {
    import numbers._
    val zero = Zero()
    println("underlying rep: " + zero)

    zero match {
      case Succ(_) => println("test1 fail")
      case Zero() => println("test1 OK")
    }

    zero match {
      case _: Succ => println("test2 fail")
      case _: Zero => println("test2 OK")
    }

    println()

    val three = Succ(Succ(zero)).succ

    println("underlying rep: " + three)

    three match {
      case Zero() => println("test3 fail")
      case s @ Succ(pred) =>
        println("test3 OK")
        println(s"Succ($pred) = ${s.value}")
    }

    three match {
      case _: Zero => println("test4 fail")
      case s: Succ =>
        println("test4 OK")
        println(s"Succ(${s.pred}) = ${s.value}")
    }
  }

}

abstract class Numbers {

  // === Nat ==========================================
  // Represents:
  //   trait Nat
  //   case object Zero extends Nat
  //   case class Succ(pred: Nat) extends Nat

  type Nat
  implicit def natAbstractTypeTag: AbstractTypeTag[Nat]

  trait AbstractNat  {
    def value: Int
    def succ: Succ
  }
  implicit def NatDeco(nat: Nat): AbstractNat

  // --- Zero ----------------------------------------

  type Zero <: Nat

  implicit def zeroAbstractTypeTag: AbstractTypeTag[Zero]

  val Zero: ZeroExtractor
  abstract class ZeroExtractor {
    def apply(): Zero
    def unapply(zero: Zero): Boolean
  }

  // --- Succ ----------------------------------------

  type Succ <: Nat

  implicit def succAbstractTypeTag: AbstractTypeTag[Succ]

  val Succ: SuccExtractor
  abstract class SuccExtractor {
    def apply(nat: Nat): Succ
    def unapply(x: Succ): Option[Nat]
  }

  trait AbstractSucc {
    def pred: Nat
  }
  implicit def SuccDeco(succ: Succ): AbstractSucc

}

object CaseClassImplementation extends Numbers {

  sealed trait N
  final object Z extends N { override def toString: String = "Z" }
  final case class S(n: N) extends N

  // === Nat ==========================================

  type Nat = N

  def natAbstractTypeTag: AbstractTypeTag[Nat] = AbstractTypeTag.classTag

  implicit def NatDeco(nat: Nat): AbstractNat = new AbstractNat {
    def value: Int = nat match {
      case Succ(n) => 1 + n.value
      case _ => 0
    }
    def succ: Succ = Succ(nat)
  }

  // --- Zero ----------------------------------------

  type Zero = Z.type

  def zeroAbstractTypeTag: AbstractTypeTag[Zero] = AbstractTypeTag.classTag

  val Zero: ZeroExtractor = new ZeroExtractor {
    def apply(): Zero = Z
    def unapply(zero: Zero): Boolean = true // checked by class tag before calling the unapply
  }

  // --- Succ ----------------------------------------

  type Succ = S

  def succAbstractTypeTag: AbstractTypeTag[Succ] = AbstractTypeTag.classTag

  val Succ: SuccExtractor = new SuccExtractor {
    def apply(nat: Nat): Succ = S(nat)
    def unapply(succ: Succ): Option[Nat] = Some(succ.n) // checked by class tag before calling the unapply
  }

  def SuccDeco(succ: Succ): AbstractSucc = new AbstractSucc {
    def pred: Nat = succ.n
  }

}


object IntImplementation extends Numbers {

  // === Nat ==========================================

  type Nat = Int

  def natAbstractTypeTag: AbstractTypeTag[Nat] = intAbstractTypeTag(_ >= 0)

  implicit def NatDeco(nat: Nat): AbstractNat = new AbstractNat {
    def value: Int = nat
    def succ: Succ = nat + 1
  }

  // --- Zero ----------------------------------------

  type Zero = Int

  def zeroAbstractTypeTag: AbstractTypeTag[Zero] = intAbstractTypeTag(_ == 0)

  val Zero: ZeroExtractor = new ZeroExtractor {
    def apply(): Zero = 0
    def unapply(zero: Zero): Boolean = true // checked by class tag before calling the unapply
  }

  // --- Succ ----------------------------------------

  type Succ = Int

  def succAbstractTypeTag: AbstractTypeTag[Succ] = intAbstractTypeTag(_ > 0)

  val Succ: SuccExtractor = new SuccExtractor {
    def apply(nat: Nat): Succ = nat + 1
    def unapply(succ: Succ): Option[Nat] = Some(succ - 1) // checked by class tag before calling the unapply
  }

  def SuccDeco(succ: Succ): AbstractSucc = new AbstractSucc {
    def pred: Nat = succ - 1
  }

  private def intAbstractTypeTag(cond: Int => Boolean): AbstractTypeTag[Int] = new AbstractTypeTag[Int] {
    def unapply(x: Any): Option[Int] = x match {
      case i: Int if cond(i) => Some(i)
      case _ => None
    }
  }

}

object UnboundedIntImplementation extends Numbers {

  // === Nat ==========================================

  type Nat = Any // Int | BigInt

  def natAbstractTypeTag: AbstractTypeTag[Nat] = new AbstractTypeTag[Any] {
    def unapply(x: Any): Option[Nat] = x match {
      case i: Int if i >= 0 => Some(i)
      case i: BigInt if i > Int.MaxValue => Some(i)
      case _ => None
    }
  }

  implicit def NatDeco(nat: Nat): AbstractNat = new AbstractNat {
    def value: Int = nat match {
      case nat: Int => nat
      case _ => throw new Exception("Number too large: " + nat)
    }
    def succ: Succ = nat match {
      case nat: Int =>
        if (nat == Integer.MAX_VALUE) BigInt(nat) + 1
        else nat + 1
      case nat: BigInt => nat + 1
    }
  }

  // --- Zero ----------------------------------------

  type Zero = Int

  def zeroAbstractTypeTag: AbstractTypeTag[Zero] = new AbstractTypeTag[Int] {
    def unapply(x: Any): Option[Int] = if (x == 0) Some(0) else None
  }

  object Zero extends ZeroExtractor {
    def apply(): Zero = 0
    def unapply(zero: Zero): Boolean = true // checked by class tag before calling the unapply
  }

  // --- Succ ----------------------------------------

  type Succ = Any // Int | BigInt

  def succAbstractTypeTag: AbstractTypeTag[Succ] = new AbstractTypeTag[Any] {
    def unapply(x: Any): Option[Succ] = x match {
      case i: Int if i > 0 => Some(i)
      case i: BigInt if i > Int.MaxValue => Some(i)
      case _ => None
    }
  }

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Succ = nat.succ
    def unapply(succ: Succ): Option[Nat] = Some(succ.pred) // succ > 0 checked by class tag before calling the unapply
  }

  implicit def SuccDeco(succ: Succ): AbstractSucc = new AbstractSucc {
    def pred: Nat = succ match {
      case succ: Int => succ - 1 // succ > 0 checked by class tag before calling the unapply
      case succ: BigInt =>
        val n = succ - 1
        if (n.isValidInt) n.intValue()
        else n
    }
  }

}
