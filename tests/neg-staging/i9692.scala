import scala.quoted.*
import scala.quoted.staging.*

object Test extends App {

  // make available the necessary compiler for runtime code generation
  given Compiler = Compiler.make(getClass.getClassLoader)

  run {
    val expr: Expr[Int] = '{ var x = 1; x = 2; 42 }

    expr match {
      case '{ var x: Int = $binding; $body(x): Int } => // error
    val res = Expr.betaReduce('{ $body(4) })
    println(res.show)
    res
      case _ => println(expr.show); '{0}
    }
  }
}
