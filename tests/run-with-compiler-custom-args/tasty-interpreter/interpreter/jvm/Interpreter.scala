package scala.tasty.interpreter
package jvm

import scala.tasty.interpreter.jvm.JVMReflection
import scala.tasty.Reflection

class Interpreter[R <: Reflection & Singleton](reflect0: R) extends TreeInterpreter[R](reflect0) {
  import reflect._

  // All references are represented by themselfs and values are boxed
  type AbstractAny = Any

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
                    val evaluatedArgs = args1.map(arg => LocalValue.valFrom(arg))

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
          val argss2 = evaluatedArgss(argss)
          // argss2.foreach(println)
          jvmReflection.interpretStaticMethodCall(fn.symbol.owner, fn.symbol, argss2)
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

  def interpretUnit(): AbstractAny = ().asInstanceOf[Object]

  def interpretLiteral(const: Constant)(implicit env: Env): AbstractAny = const.value

  def interpretIsInstanceOf(o: AbstractAny, tpt: TypeTree)(implicit env: Env): AbstractAny =
    jvmReflection.getClassOf(tpt.symbol).isInstance(o)

  def interpretAsInstanceOf(o: AbstractAny, tpt: TypeTree)(implicit env: Env): AbstractAny =
    jvmReflection.getClassOf(tpt.symbol).cast(o)

  def interpretEqEq(x: AbstractAny, y: AbstractAny): AbstractAny = x == y

  def interpretPrivitiveLt(x: AbstractAny, y: AbstractAny): AbstractAny = coerce(x, y)(_.lt(_, _))
  def interpretPrivitiveGt(x: AbstractAny, y: AbstractAny): AbstractAny = coerce(x, y)(_.gt(_, _))
  def interpretPrivitivePlus(x: AbstractAny, y: AbstractAny): AbstractAny = coerce(x, y)(_.plus(_, _))
  def interpretPrivitiveMinus(x: AbstractAny, y: AbstractAny): AbstractAny = coerce(x, y)(_.minus(_, _))

  private def coerce(x: AbstractAny, y: AbstractAny)(body: (Numeric[AbstractAny], AbstractAny, AbstractAny) => AbstractAny): AbstractAny = {
    def getNumericFor[T](implicit x: Numeric[T]): Numeric[AbstractAny] =
      x.asInstanceOf[Numeric[Any]]
    // TODO complete: Float Double Char
    x match {
      case x: Byte =>
        y match {
          case y: Byte  => body(getNumericFor[Byte], x, y)
          case y: Short => body(getNumericFor[Short], x.toShort, y)
          case y: Int   => body(getNumericFor[Int], x.toInt, y)
          case y: Long  => body(getNumericFor[Long], x.toLong, y)
        }
      case x: Short =>
        y match {
          case y: Byte  => body(getNumericFor[Short], x, y.toShort)
          case y: Short => body(getNumericFor[Short], x, y)
          case y: Int   => body(getNumericFor[Int], x.toInt, y)
          case y: Long  => body(getNumericFor[Long], x.toLong, y)
        }
      case x: Int =>
        y match {
          case y: Byte  => body(getNumericFor[Int], x, y.toInt)
          case y: Short => body(getNumericFor[Int], x, y.toInt)
          case y: Int   => body(getNumericFor[Int], x, y)
          case y: Long  => body(getNumericFor[Long], x.toLong, y)
        }
      case x: Long =>
        y match {
          case y: Byte  => body(getNumericFor[Long], x, y.toLong)
          case y: Short => body(getNumericFor[Long], x, y.toLong)
          case y: Int   => body(getNumericFor[Long], x, y.toLong)
          case y: Long  => body(getNumericFor[Long], x, y)
        }
    }
  }


}