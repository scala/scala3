package dotty.tools.repl

import java.io.PrintStream

class ReplMain(
  settings: Array[String] = Array.empty,
  out: PrintStream = Console.out,
  classLoader: Option[ClassLoader] = Some(getClass.getClassLoader),
  predefCode: String = "",
  testCode: String = ""
):
  def run(bindings: ReplMain.Bind[_]*): Any =
    try
      ReplMain.currentBindings.set(bindings.map{bind => bind.name -> bind.value}.toMap)

      val bindingsPredef = bindings
        .map { case bind =>
          s"def ${bind.name}: ${bind.typeName.value} = dotty.tools.repl.ReplMain.currentBinding[${bind.typeName.value}](\"${bind.name}\")"
        }
        .mkString("\n")

      val fullPredef =
        ReplDriver.pprintImport +
        (if bindingsPredef.nonEmpty then s"\n$bindingsPredef\n" else "") +
        (if predefCode.nonEmpty then s"\n$predefCode\n" else "")

      val driver = new ReplDriver(settings, out, classLoader, fullPredef)

      if (testCode == "") driver.tryRunning
      else driver.runUntilQuit(using driver.initialState)(
        new java.io.ByteArrayInputStream(testCode.getBytes())
      )
      ()
    finally
      ReplMain.currentBindings.set(null)


object ReplMain:
  final case class TypeName[A](value: String)
  object TypeName extends TypeNamePlatform

  import scala.quoted._

  trait TypeNamePlatform:
    inline given [A]: TypeName[A] = ${TypeNamePlatform.impl[A]}

  object TypeNamePlatform:
    def impl[A](using t: Type[A], ctx: Quotes): Expr[TypeName[A]] =
    '{TypeName[A](${Expr(Type.show[A])})}


  case class Bind[T](name: String, value: () => T)(implicit val typeName: TypeName[T])
  object Bind:
    implicit def ammoniteReplArrowBinder[T](t: (String, T))(implicit typeName: TypeName[T]): Bind[T] = {
      Bind(t._1, () => t._2)(typeName)
    }

  def currentBinding[T](s: String): T = currentBindings.get().apply(s).apply().asInstanceOf[T]

  private val currentBindings = new ThreadLocal[Map[String, () => Any]]()
