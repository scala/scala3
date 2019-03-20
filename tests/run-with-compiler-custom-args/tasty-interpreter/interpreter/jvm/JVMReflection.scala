package scala.tasty.interpreter.jvm

import scala.tasty.Reflection

class JVMReflection[R <: Reflection & Singleton](val reflect: R) {
  import reflect._
  import java.lang.reflect.{InvocationTargetException, Method}
  private val classLoader: ClassLoader = getClass.getClassLoader

  // taken from StdNames
  final val MODULE_INSTANCE_FIELD      = "MODULE$"

  def loadModule(sym: Symbol): Object = {

    sym.owner match {
      case IsPackageDefSymbol(_) =>
        val moduleClass = getClassOf(sym)
        moduleClass.getField(MODULE_INSTANCE_FIELD).get(null)
      case _ =>
        // nested object in an object
        // val clazz = loadClass(sym.fullNameSeparated(FlatName))
        // clazz.getConstructor().newInstance().asInstanceOf[Object]
        ???
    }
  }

  def getClassOf(sym: Symbol): Class[_] = {
    sym.fullName match {
      case "scala.Boolean" => classOf[java.lang.Boolean]
      case "scala.Short" => classOf[java.lang.Short]
      case "scala.Char" => classOf[java.lang.Character]
      case "scala.Int" => classOf[java.lang.Integer]
      case "scala.Long" => classOf[java.lang.Long]
      case "scala.Float" => classOf[java.lang.Float]
      case "scala.Double" => classOf[java.lang.Double]
      case _ => loadClass(sym.fullName)
    }
  }

  def loadClass(name: String): Class[_] = {
    try classLoader.loadClass(name)
    catch {
      case _: ClassNotFoundException =>
        val msg = s"Could not find class $name in classpath$extraMsg"
        throw new Exception(msg)
    }
  }

  def interpretStaticVal(moduleClass: Symbol, fn: Symbol): Object = {
    val inst = loadModule(moduleClass)
    val name = fn.name
    val method = getMethod(inst.getClass, name, Nil)
    method.invoke(inst)
  }

  def interpretStaticMethodCall(moduleClass: Symbol, fn: Symbol, args: List[Object]): Object = {
    // TODO can we use interpretMethodCall instead?
    val inst = loadModule(moduleClass)
    val method = getMethod(inst.getClass, fn.name, paramsSig(fn))
    method.invoke(inst, args: _*)
  }

  def interpretMethodCall(inst: Object, fn: Symbol, args: List[Object]): Object = {
    val method = getMethod(inst.getClass, fn.name, paramsSig(fn))
    method.invoke(inst, args: _*)
  }

  def interpretNew(fn: Symbol, args: List[Object]): Object = {
    val clazz = getClassOf(fn.owner)
    val constr = clazz.getConstructor(paramsSig(fn): _*)
    constr.newInstance(args: _*).asInstanceOf[Object]
  }

  def getMethod(clazz: Class[_], name: String, paramClasses: List[Class[_]]): Method = {
    try clazz.getMethod(name, paramClasses: _*)
    catch {
      case _: NoSuchMethodException =>
        val msg = s"Could not find method ${clazz.getCanonicalName}.$name with parameters ($paramClasses%, %)$extraMsg"
        throw new Exception(msg)
    }
  }

  private def paramsSig(sym: Symbol): List[Class[_]] = {
    sym.asDefDef.signature.paramSigs.map { param =>
      def javaArraySig(name: String): String = {
        if (name.endsWith("[]")) "[" + javaArraySig(name.dropRight(2))
        else name match {
          case "scala.Boolean" => "Z"
          case "scala.Byte" => "B"
          case "scala.Short" => "S"
          case "scala.Int" => "I"
          case "scala.Long" => "J"
          case "scala.Float" => "F"
          case "scala.Double" => "D"
          case "scala.Char" => "C"
          case paramName => "L" + paramName + ";"
        }
      }

      def javaSig(name: String): String =
        if (name.endsWith("[]")) javaArraySig(name) else name

      if (param == "scala.Boolean") classOf[Boolean]
      else if (param == "scala.Byte") classOf[Byte]
      else if (param == "scala.Char") classOf[Char]
      else if (param == "scala.Short") classOf[Short]
      else if (param == "scala.Int") classOf[Int]
      else if (param == "scala.Long") classOf[Long]
      else if (param == "scala.Float") classOf[Float]
      else if (param == "scala.Double") classOf[Double]
      else java.lang.Class.forName(javaSig(param), false, classLoader)
    }
  }

  private def extraMsg = ". The most common reason for that is that you apply macros in the compilation run that defines them"
}
