sealed trait Expression:
  def eval(): Int
case class Number(n: Int) extends Expression:
  def eval(): Int = n
case class Plus(e1: Expression, e2: Expression) extends Expression:
  def eval(): Int = e1.eval() + e2.eval()

////

@main def sum(n1: Int, n2: Int) =
  val x1 = Number(n1)
  val x2 = Number(n2)
  val expr = Plus(x1, x2)
  println(s"Expression: $expr")
  val result = expr.eval()
  println(s"Calculated: $result")

////

import scala.annotation.{ MainAnnotation, experimental }
import scala.annotation.MainAnnotation.{ Info, Parameter }
import scala.util.CommandLineParser.FromString

@experimental class showAndEval extends MainAnnotation[FromString, Expression]:
  def command(info: Info, args: Seq[String]): Option[Seq[String]] =
    assert(info.parameters.forall(param => param.typeName == "Number"), "Only Number parameters allowed")
    println(s"executing ${info.name} with inputs: ${args.mkString(" ")}")
    Some(args)

  def argGetter[T](param: Parameter, arg: String, defaultArgument: Option[() => T])(using parser: FromString[T]): () => T =
    () => parser.fromString(arg)

  def varargGetter[T](param: Parameter, args: Seq[String])(using parser: FromString[T]): () => Seq[T] =
    () => args.map(arg => parser.fromString(arg))

  def run(program: () => Expression): Unit =
    val expr = program()
    println(s"Expression: $expr")
    val result = expr.eval()
    println(s"Calculated: $result")
end showAndEval

given FromString[Number] = (x: String) => Number(x.toInt)

////

@showAndEval def sum2(x1: Number, x2: Number): Expression =
  sumAll(x1, x2)

@showAndEval def sumAll(xs: Number*): Expression =
  if xs.isEmpty then Number(0)
  else xs.tail.fold[Expression](xs.head)(Plus)

////

@main def Test: Unit =
  def callMain(name: String, args: String*): Unit =
    val clazz = Class.forName(name)
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args.toArray)
  callMain("sum", "1", "2")
  callMain("sum2", "2", "3")
  callMain("sumAll", "1", "2", "3")
end Test
