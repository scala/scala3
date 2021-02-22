#!/usr/bin/env scala
export STUFF=nada
#lots of other stuff that isn't valid scala
!#
object Zoo {
  def main(args: Array[String]): Unit =
    printf("script.name: %s\n",sys.props("script.name"))
    printf("mainClassFromStack: %s\n",mainFromStack)
    assert(mainFromStack == "Zoo",s"fromStack[$mainFromStack]")

  lazy val mainFromStack:String = {
    val result = new java.io.StringWriter()
    new RuntimeException("stack").printStackTrace(new java.io.PrintWriter(result))
    val stack = result.toString.split("[\r\n]+").toList
    // for( s <- stack ){ System.err.printf("[%s]\n",s) }
    val shortStack = stack.filter { str => str.contains(".main(") && ! str.contains("$") }.map {
      _.replaceAll("[.].*","").replaceAll("\\s+at\\s+","")
    }
    // for( s <- shortStack ){ System.err.printf("[%s]\n",s) }
    shortStack.take(1).mkString("|")
  }
}
