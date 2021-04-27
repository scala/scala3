import scala.reflect.ClassTag

object Test {
  def main(args: Array[String]): Unit = {
    println("CaseClassImplementation")
    testInterface(CaseClassImplementation)

    println()

    println("ListImplementation")
    testInterface(ListImplementation)
  }

  def testInterface(arithmetic: Arithmetic): Unit = {
    import arithmetic.*
    val const1 = Constant(1)
    println("underlying rep: " + const1.getClass)
    println(const1.eval)

    const1 match {
      case AppliedOp(_, _, _) =>
        println("test1 fail")
      case c @ Constant(n) =>
        println("test1 OK")
        println(s"$n = ${c.eval}")
    }

    const1 match {
      case _: AppliedOp =>
        println("test2 fail")
      case c: Constant =>
        println("test2 OK")
        println(s"${c.num} = ${c.eval}")
    }
    println()

    // 1 + (2 * 3)
    val applied = AppliedOp(Op.Puls(), Constant(1), AppliedOp(Op.Mult(), Constant(2), Constant(3)))

    println("underlying rep: " + applied.getClass)
    println(applied.eval)

    applied match {
      case c @ Constant(n) =>
        println("test3 fail")
      case a @ AppliedOp(op, x, y) =>
        println("test3 OK")
        println(s"AppliedOp($op, $x, $y) = ${a.eval}")
    }

    applied match {
      case c: Constant =>
        println("test4 fail")
      case a: AppliedOp =>
        println("test4 OK")
        println(s"AppliedOp(${a.op}, ${a.lhs}, ${a.rhs}) = ${a.eval}")
    }

  }
}

abstract class Arithmetic {

  // === Numbers ==========================================
  // Represents:
  //   trait Number
  //   case class Constant(n: Int) extends Number
  //   case class AppliedOp(op: Op, lhs: Number, rhs: Number) extends Number

  type Number
  implicit def numberClassTag: ClassTag[Number]

  trait AbstractNumber  {
    def thisNumber: Number
    def eval: Int = thisNumber match {
      case Constant(n) => n
      case AppliedOp(op, x, y) => op(x, y)
    }
  }
  implicit def NumberDeco(t: Number): AbstractNumber

  // --- Constant ----------------------------------------

  type Constant <: Number
  implicit def constantClassTag: ClassTag[Constant]

  val Constant: ConstantExtractor
  abstract class ConstantExtractor {
    def apply(x: Int): Constant
    def unapply(x: Constant): Option[Int]
  }
  trait AbstractConstant {
    def num: Int
  }
  implicit def ConstantDeco(t: Constant): AbstractConstant

  // --- AppliedOp ----------------------------------------

  type AppliedOp <: Number
  implicit def appliedOpClassTag: ClassTag[AppliedOp]

  trait AbstractAppliedOp {
    def op: Op
    def lhs: Number
    def rhs: Number
  }
  implicit def AppliedOpDeco(t: AppliedOp): AbstractAppliedOp

  val AppliedOp: AppliedOpExtractor
  abstract class AppliedOpExtractor {
    def apply(op: Op, x: Number, y: Number): AppliedOp
    def unapply(x: AppliedOp): Option[(Op, Number, Number)]
  }

  // === Operations =======================================
  // Represents:
  //   trait Op
  //   case object Puls extends Op
  //   case object Mult extends Op

  type Op
  implicit def opClassTag: ClassTag[Op]

  trait AbstractOp {
    def thisOp: Op
    def apply(x: Number, y: Number): Int = thisOp match {
      case Op.Puls() => x.eval + y.eval
      case Op.Mult() => x.eval * y.eval
    }
  }
  implicit def OpDeco(t: Op): AbstractOp

  val Op: OpModule
  abstract class OpModule {
    val Puls: PulsExtractor
    abstract class PulsExtractor {
      def apply(): Op
      def unapply(x: Op): Boolean
    }

    val Mult: MultExtractor
    abstract class MultExtractor {
      def apply(): Op
      def unapply(x: Op): Boolean
    }
  }
}

object CaseClassImplementation extends Arithmetic {

  // === Numbers ==========================================
  // Represented as case classes

  sealed trait Num
  final case class Const(n: Int) extends Num
  final case class App(op: Op, x: Num, y: Num) extends Num

  type Number = Num

  def numberClassTag: ClassTag[Number] = implicitly

  def NumberDeco(t: Number): AbstractNumber = new AbstractNumber {
    def thisNumber: Number = t
  }

  // --- Constant ----------------------------------------

  type Constant = Const
  def constantClassTag: ClassTag[Constant] = implicitly

  def ConstantDeco(const: Constant): AbstractConstant = new AbstractConstant {
    def num: Int = const.n
  }

  object Constant extends ConstantExtractor {
    def apply(x: Int): Constant = Const(x)
    def unapply(x: Constant): Option[Int] = Some(x.n)
  }

  // --- AppliedOp ----------------------------------------

  def AppliedOpDeco(t: AppliedOp): AbstractAppliedOp = new AbstractAppliedOp {
    def op: Op = t.op
    def lhs: Number = t.x
    def rhs: Number = t.y
  }

  type AppliedOp = App
  def appliedOpClassTag: ClassTag[AppliedOp] = implicitly

  object AppliedOp extends AppliedOpExtractor {
    def apply(op: Op, x: Number, y: Number): AppliedOp = App(op, x, y)
    def unapply(app: AppliedOp): Option[(Op, Number, Number)] = Some((app.op, app.x, app.y))
  }

  // === Operations =======================================
  // Represented as case classes

  sealed trait Operation
  case object PlusOp extends Operation
  case object MultOp extends Operation

  type Op = Operation
  def opClassTag: ClassTag[Op] = implicitly

  def OpDeco(t: Op): AbstractOp = new AbstractOp {
    def thisOp: Op = t
  }

  object Op extends OpModule {
    object Puls extends PulsExtractor {
      def apply(): Op = PlusOp
      def unapply(x: Op): Boolean = x == PlusOp
    }
    object Mult extends MultExtractor {
      def apply(): Op = MultOp
      def unapply(x: Op): Boolean = x == MultOp
    }
  }
}

object ListImplementation extends Arithmetic {
  // Logically represented as:
  //   type Number <: List[Any]
  //   type Constant <: Number // List(n: Int)
  //   type AppliedOp <: Number // List(op: Op, lhs: Number, rhs: Number)
  //
  //   type Op <: List[Any] // List(id: "+" | "*")

  // === Numbers ==========================================

  type Number = List[Any]

  def numberClassTag: ClassTag[Number] = new ClassTag[Number] {
    def runtimeClass: Class[_] = classOf[List[_]]
    override def unapply(x: Any): Option[List[Any]] = x match {
      case ls: List[Any] if ls.length == 3 || (ls.length == 1 && ls(0).isInstanceOf[Int]) =>
        // Test that it is one of:
        //   type Constant <: Number // List(n: Int)
        //   type AppliedOp <: Number // List(op: Op, lhs: Number, rhs: Number)
        Some(ls)
      case _ => None
    }
  }

  def NumberDeco(t: Number): AbstractNumber = new AbstractNumber {
    def thisNumber: Number = t
  }

  // --- Constant ----------------------------------------

  type Constant = List[Any] // List(n: Int)
  def constantClassTag: ClassTag[Constant] = new ClassTag[Constant] {
    def runtimeClass: Class[_] = classOf[List[_]]
    override def unapply(x: Any): Option[List[Any]] = x match {
      case ls: List[Any] if ls.length == 1 && ls(0).isInstanceOf[Int] =>
        // Test that it is:
        //   type Constant <: Number // List(n: Int)
        Some(ls)
      case _ => None
    }
  }

  def ConstantDeco(const: Constant): AbstractConstant = new AbstractConstant {
    def num: Int = const(0).asInstanceOf[Int]
  }

  object Constant extends ConstantExtractor {
    def apply(x: Int): Constant = List(x)
    def unapply(x: Constant): Option[Int] = Some(ConstantDeco(x).num)
  }

  // --- AppliedOp ----------------------------------------

  def AppliedOpDeco(t: AppliedOp): AbstractAppliedOp = new AbstractAppliedOp {
    def op: Op = t(0).asInstanceOf[Op]
    def lhs: Number = t(1).asInstanceOf[Number]
    def rhs: Number = t(2).asInstanceOf[Number]
  }

  type AppliedOp = List[Any] // List(op: Op, lhs: Number, rhs: Number)
  def appliedOpClassTag: ClassTag[AppliedOp] = new ClassTag[AppliedOp] {
    def runtimeClass: Class[_] = classOf[List[_]]
    override def unapply(x: Any): Option[List[Any]] = x match {
      case ls: List[Any] if ls.length == 3 =>
        // Test that it is:
        //   type AppliedOp <: Number // List(op: Op, lhs: Number, rhs: Number)
        Some(ls)
      case _ => None
    }
  }

  object AppliedOp extends AppliedOpExtractor {
    def apply(op: Op, x: Number, y: Number): AppliedOp = List(op, x, y)
    def unapply(app: AppliedOp): Option[(Op, Number, Number)] = {
      val app2 = AppliedOpDeco(app)
      Some((app2.op, app2.lhs, app2.rhs))
    }
  }

  // === Operations =======================================

  type Op = List[Any]
  def opClassTag: ClassTag[Op] = new ClassTag[Constant] {
    def runtimeClass: Class[_] = classOf[List[_]]
    override def unapply(x: Any): Option[List[Any]] = x match {
      case op @ (("+" | "*") :: Nil) =>
      // Test that it is:
      //   type Op <: List[Any] // List(id: "+" | "*")
      Some(op)
      case _ => None
    }
  }

  def OpDeco(t: Op): AbstractOp = new AbstractOp {
    def thisOp: Op = t
  }

  object Op extends OpModule {
    object Puls extends PulsExtractor {
      def apply(): Op = List("+")
      def unapply(x: Op): Boolean = x(0) == "+"
    }
    object Mult extends MultExtractor {
      def apply(): Op = List("*")
      def unapply(x: Op): Boolean = x(0) == "*"
    }
  }
}