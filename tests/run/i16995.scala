import scala.language.dynamics
import scala.reflect.Selectable.reflectiveSelectable

object Test {
  class Num(val i: Int) extends AnyVal

  def show(x: Int | Num | Seq[Int | Num]): String = x match
    case i: Int => i.toString
    case num: Num => num.i.toString
    case seq: Seq[Int | Num] => seq.map(show).mkString(" ")

  trait Nonreflective extends Selectable:
    def selectDynamic(name: String): String = name
    def applyDynamic(name: String)(args: (Int | Num | Seq[Int | Num])*): String =
      val argsString = args.map(show).mkString(" ", " ", "")
      s"${name}${argsString}"

  trait Dynamic0 extends Dynamic:
    def selectDynamic(name: String): String = name

  trait Dynamic1 extends Dynamic:
    def applyDynamic(name: String)(args1: (Int | Num | Seq[Int | Num])*): String =
      val argsString = args1.map(show).mkString(" ", " ", "")
      s"${name}${argsString}"

  trait Dynamic2 extends Dynamic:
    def applyDynamic(name: String)(args1: (Int | Num)*)(args2: (Int | Num)*): String =
      val argsString = (args1 ++ args2).map(show).mkString(" ", " ", "")
      s"${name}${argsString}"

  trait Dynamic3 extends Dynamic:
    def applyDynamic(name: String)(args1: (Int | Num)*)(args2: (Int | Num)*)(args3: (Int | Num)*): String =
      val argsString = (args1 ++ args2 ++ args3).map(show).mkString(" ", " ", "")
      s"${name}${argsString}"

  type Api = {
    def foo: String
    def foo0(): String
    def fooI(i: Int): String
    def fooN(n: Num): String
    def fooII(i1: Int, i2: Int): String
    def fooNN(n1: Num, n2: Num): String
    def fooIvarargs(is: Int*): String
    def fooNvarargs(ns: Num*): String
    def fooIseq(is: Seq[Int]): String
    def fooNseq(ns: Seq[Num]): String
    def fooIIvarargs(i1: Int, is: Int*): String
    def fooNNvarargs(n1: Num, ns: Num*): String
    def fooI_I(i1: Int)(i2: Int): String
    def fooN_N(n1: Num)(n2: Num): String
    def foo0_II()(i1: Int, i2: Int): String
    def foo0_NN()(n1: Num, n2: Num): String
    def foo0_Ivarargs()(is: Int*): String
    def foo0_Nvarargs()(ns: Num*): String
    def foo0_I_I()(i1: Int)(i2: Int): String
    def foo0_N_N()(n1: Num)(n2: Num): String
  }

  class ClassImpl {
    def foo: String = "foo"
    def foo0(): String = "foo0"
    def fooI(i: Int): String = s"fooI ${i}"
    def fooN(n: Num): String = s"fooN ${n.i}"
    def fooII(i1: Int, i2: Int): String = s"fooII ${i1} ${i2}"
    def fooNN(n1: Num, n2: Num): String = s"fooNN ${n1.i} ${n2.i}"
    def fooIvarargs(is: Int*): String = s"fooIvarargs${is.mkString(" ", " ", "")}"
    def fooNvarargs(ns: Num*): String = s"fooNvarargs${ns.map(_.i).mkString(" ", " ", "")}"
    def fooIseq(is: Seq[Int]): String = s"fooIseq${is.mkString(" ", " ", "")}"
    def fooNseq(ns: Seq[Num]): String = s"fooNseq${ns.map(_.i).mkString(" ", " ", "")}"
    def fooIIvarargs(i1: Int, is: Int*): String = s"fooIIvarargs ${i1}${is.mkString(" ", " ", "")}"
    def fooNNvarargs(n1: Num, ns: Num*): String = s"fooNNvarargs ${n1.i}${ns.map(_.i).mkString(" ", " ", "")}"
    def fooI_I(i1: Int)(i2: Int): String = s"fooI_I ${i1} ${i2}"
    def fooN_N(n1: Num)(n2: Num): String = s"fooN_N ${n1.i} ${n2.i}"
    def foo0_II()(i1: Int, i2: Int): String = s"foo0_II ${i1} ${i2}"
    def foo0_NN()(n1: Num, n2: Num): String = s"foo0_NN ${n1.i} ${n2.i}"
    def foo0_Ivarargs()(is: Int*): String = s"foo0_Ivarargs${is.mkString(" ", " ", "")}"
    def foo0_Nvarargs()(ns: Num*): String = s"foo0_Nvarargs${ns.map(_.i).mkString(" ", " ", "")}"
    def foo0_I_I()(i1: Int)(i2: Int): String = s"foo0_I_I ${i1} ${i2}"
    def foo0_N_N()(n1: Num)(n2: Num): String = s"foo0_N_N ${n1.i} ${n2.i}"
  }


  def main(args: Array[String]): Unit = {
    val reflective: Api = new ClassImpl()
    val nonreflective: Nonreflective & Api = (new Nonreflective {}).asInstanceOf[Nonreflective & Api]
    val dynamic0 = new Dynamic0 {}
    val dynamic1 = new Dynamic1 {}
    val dynamic2 = new Dynamic2 {}
    val dynamic3 = new Dynamic3 {}

    println(reflective.foo)
    println(reflective.foo0())
    println(reflective.fooI(1))
    println(reflective.fooN(new Num(1)))
    println(reflective.fooII(1, 2))
    println(reflective.fooNN(new Num(1), new Num(2)))
    println(reflective.fooIvarargs(1, 2))
    println(reflective.fooNvarargs(new Num(1), new Num(2)))
    println(reflective.fooIseq(Seq(1, 2)))
    println(reflective.fooNseq(Seq(new Num(1), new Num(2))))
    println(reflective.fooIIvarargs(1, 2))
    println(reflective.fooNNvarargs(new Num(1), new Num(2)))
    println(reflective.fooI_I(1)(2))
    println(reflective.fooN_N(new Num(1))(new Num(2)))
    println(reflective.foo0_II()(1, 2))
    println(reflective.foo0_NN()(new Num(1), new Num(2)))
    println(reflective.foo0_Ivarargs()(1, 2))
    println(reflective.foo0_Nvarargs()(new Num(1), new Num(2)))
    println(reflective.foo0_I_I()(1)(2))
    println(reflective.foo0_N_N()(new Num(1))(new Num(2)))
    println()
    println(nonreflective.foo)
    println(nonreflective.foo0())
    println(nonreflective.fooI(1))
    println(nonreflective.fooN(new Num(1)))
    println(nonreflective.fooII(1, 2))
    println(nonreflective.fooNN(new Num(1), new Num(2)))
    println(nonreflective.fooIvarargs(1, 2))
    println(nonreflective.fooNvarargs(new Num(1), new Num(2)))
    println(nonreflective.fooIseq(Seq(1, 2)))
    println(nonreflective.fooNseq(Seq(new Num(1), new Num(2))))
    println(nonreflective.fooIIvarargs(1, 2, 3))
    println(nonreflective.fooNNvarargs(new Num(1), new Num(2)))
    println(nonreflective.fooI_I(1)(2))
    println(nonreflective.fooN_N(new Num(1))(new Num(2)))
    println(nonreflective.foo0_II()(1, 2))
    println(nonreflective.foo0_NN()(new Num(1), new Num(2)))
    println(nonreflective.foo0_Ivarargs()(1, 2))
    println(nonreflective.foo0_Nvarargs()(new Num(1), new Num(2)))
    println(nonreflective.foo0_I_I()(1)(2))
    println(nonreflective.foo0_N_N()(new Num(1))(new Num(2)))
    println()
    println(dynamic0.foo)
    println(dynamic1.foo0())
    println(dynamic1.fooI(1))
    println(dynamic1.fooN(new Num(1)))
    println(dynamic1.fooII(1, 2))
    println(dynamic1.fooNN(new Num(1), new Num(2)))
    println(dynamic1.fooIvarargs(1, 2))
    println(dynamic1.fooNvarargs(new Num(1), new Num(2)))
    println(dynamic1.fooIseq(Seq(1, 2)))
    println(dynamic1.fooNseq(Seq(new Num(1), new Num(2))))
    println(dynamic1.fooIIvarargs(1, 2))
    println(dynamic1.fooNNvarargs(new Num(1), new Num(2)))
    println(dynamic2.fooI_I(1)(2))
    println(dynamic2.fooN_N(new Num(1))(new Num(2)))
    println(dynamic2.foo0_II()(1, 2))
    println(dynamic2.foo0_NN()(new Num(1), new Num(2)))
    println(dynamic2.foo0_Ivarargs()(1, 2))
    println(dynamic2.foo0_Nvarargs()(new Num(1), new Num(2)))
    println(dynamic3.foo0_I_I()(1)(2))
    println(dynamic3.foo0_N_N()(new Num(1))(new Num(2)))
  }
}