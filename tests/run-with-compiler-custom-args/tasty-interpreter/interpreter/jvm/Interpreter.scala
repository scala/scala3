package scala.tasty.interpreter
package jvm

import scala.tasty.interpreter.abstr.{Ref, Eager, Lazy, Var}
import scala.tasty.interpreter.jvm.JVMReflection
import scala.tasty.Reflection

class Interpreter[R <: Reflection & Singleton](reflect0: R) extends TreeInterpreter[R](reflect0) {
  import reflect._

  val jvmReflection = new JVMReflection(reflect)

  def interpretNew(fn: Tree, argss: List[List[Term]])(implicit env: Env): Any = {
    if (fn.symbol.isDefinedInCurrentRun) {
      // Best effort to try to create a proxy
      fn.symbol.owner match {
        case IsClassSymbol(sym) =>
          val parentSymbols = sym.tree.parents.tail.map(_.asInstanceOf[TypeTree].symbol).head
          import java.lang.reflect._
          val handler: InvocationHandler = new InvocationHandler() {
            val instance = new Object

            def invoke(proxy: Object, method: Method, args: scala.Array[Object]): Object = {

              // println(method)
              val symbol = sym.methods.find(_.name == method.getName).get

              if (symbol.isDefinedInCurrentRun) {
                symbol match {
                  case IsDefSymbol(symbol) =>
                    val args1 = if (args == null) Nil else args.toList
                    val evaluatedArgs = args1.map(arg => new Eager(arg))

                    val env1 = env ++ symbol.tree.paramss.headOption.getOrElse(Nil).map(_.symbol).zip(evaluatedArgs)
                    // println(symbol.tree)
                    eval(symbol.tree.rhs.get)(env1).asInstanceOf[Object]
                }
              }
              else
                method.invoke(instance, args: _*)
            }

          }
          val proxyClass: Class[_] = Proxy.getProxyClass(getClass.getClassLoader, jvmReflection.loadClass(parentSymbols.fullName))
          proxyClass.getConstructor(classOf[InvocationHandler]).newInstance(handler);
      }
    }
    else jvmReflection.interpretNew(fn.symbol, evaluatedArgss(argss))
  }

  override def interpretCall(fn: Tree, argss: List[List[Term]])(implicit env: Env): Any = {
    if (fn.symbol.isDefinedInCurrentRun) super.interpretCall(fn, argss)
    else {
      import Term._
      // println(fn.show)
      fn.symbol match {
        // TODO: obviously
        case IsDefSymbol(sym) =>
          if (sym.name == "<init>") jvmReflection.interpretNew(sym, evaluatedArgss(argss))
          else if (sym.name == "==") eval(Term.IsSelect.unapply(fn).get.qualifier).asInstanceOf[Int] == eval(argss.head.head).asInstanceOf[Int]
          else if (sym.name == ">") eval(Term.IsSelect.unapply(fn).get.qualifier).asInstanceOf[Int] > eval(argss.head.head).asInstanceOf[Int]
          else if (sym.name == "-") eval(Term.IsSelect.unapply(fn).get.qualifier).asInstanceOf[Int] - eval(argss.head.head).asInstanceOf[Int]
          else if (sym.name == "+") eval(Term.IsSelect.unapply(fn).get.qualifier).asInstanceOf[Int] + eval(argss.head.head).asInstanceOf[Int]
          else {
            val argss2 = evaluatedArgss(argss)
            // argss2.foreach(println)
            jvmReflection.interpretStaticMethodCall(fn.symbol.owner, fn.symbol, argss2)
          }
        case _ =>
          if (fn.symbol.flags.isObject) {
            jvmReflection.loadModule(fn.symbol.asVal.moduleClass.get)
          }
          // call to a static val
          else {
            jvmReflection.interpretStaticVal(fn.symbol.owner, fn.symbol)
          }
      }
    }
  }

  def evaluatedArgss(argss: List[List[Term]])(implicit env: Env): List[Object] = argss.flatMap((a: List[Term]) => a.map(b => eval(b).asInstanceOf[Object]))

  def reflectNew(fn: Tree, argss: List[List[Term]])(implicit env: Env): Any =
    jvmReflection.interpretNew(fn.symbol, evaluatedArgss(argss))

}