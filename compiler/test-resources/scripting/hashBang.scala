#!/usr/bin/env scala
# comment
STUFF=nada
!#

def main(args: Array[String]): Unit =
  System.err.printf("mainClassFromStack: %s\n",mainFromStack)
  assert(mainFromStack.contains("hashBang"),s"fromStack[$mainFromStack]")

  lazy val mainFromStack:String = {
    val result = new java.io.StringWriter()
    new RuntimeException("stack").printStackTrace(new java.io.PrintWriter(result))
    val stack = result.toString.split("[\r\n]+").toList
    //for( s <- stack ){ System.err.printf("[%s]\n",s) }
    stack.filter { str => str.contains(".main(") }.map {
      _.replaceAll(".*[(]","").
      replaceAll("\\.main\\(.*","").
      replaceAll(".scala.*","")
    }.distinct.take(1).mkString("")
  }
