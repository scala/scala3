// scalajs: --skip
// (this is a JVM-only test)

import language.experimental.erasedDefinitions

class CanSerialize extends compiletime.Erased

object CtxFns {
  abstract class Context:
    def puts[T](t: T): Unit

  def puts[T](t: T): Context ?=> Unit = summon[Context].puts(t)

  def foo(x: Int): CanSerialize ?=> Context ?=> String = "foo"

  type Handler = Context ?=> String
  def bar(x: Int): Handler = "bar"
}

object Test:
  def main(args: Array[String]): Unit =
    classOf[CtxFns.type].getDeclaredMethods.sortBy(_.getName).foreach(m =>
      println(m)
      println(m.getParameterTypes().mkString(",") + " -> " + m.getReturnType.toString)
      println(m.getGenericParameterTypes().mkString(",") + " -> " + m.getGenericReturnType.toString)
    )
