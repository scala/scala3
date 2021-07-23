#!/usr/bin/env scala
# comment
STUFF=nada
!#
// everything above this point should be ignored by the compiler
def main(args: Array[String]): Unit =
  args.zipWithIndex.foreach { case (arg,i) => printf("arg %d: [%s]\n",i,arg) }
  System.err.printf("mainClassFromStack: %s\n",mainFromStack)
  assert(mainFromStack.contains("hashBang"),s"fromStack[$mainFromStack]")

  lazy val mainFromStack:String = {
    val result = new java.io.StringWriter()
    new RuntimeException("stack").printStackTrace(new java.io.PrintWriter(result))
    val stack = result.toString.split("[\r\n]+").toList
    if verbose then for( s <- stack ){ System.err.printf("[%s]\n",s) }
    stack.filter { str => str.contains(".main(") }.map {
      // derive main class name from stack when main object is NOT declared in source
      _.replaceAll("[.].*","").
      replaceAll("\\s+at\\s+","")
    }.distinct.take(1).mkString("")
  }

  lazy val verbose = Option(System.getenv("DOTC_VERBOSE")) match
    case None => false
    case _ => true
