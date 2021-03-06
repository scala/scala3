#!/usr/bin/env scala
export STUFF=nada
#lots of other stuff that isn't valid scala
!#
// everything above this point should be ignored by the compiler
object Zoo {
  def main(args: Array[String]): Unit =
    args.zipWithIndex.foreach { case (arg,i) => printf("arg %d: [%s]\n",i,arg) }
    printf("mainClassFromStack: %s\n",mainClassFromStack)
    assert(mainClassFromStack == "Zoo",s"fromStack[$mainClassFromStack]")

  lazy val mainClassFromStack:String = {
    val result = new java.io.StringWriter()
    new RuntimeException("stack").printStackTrace(new java.io.PrintWriter(result))
    val stack = result.toString.split("[\r\n]+").toList
    if verbose then for( s <- stack ){ System.err.printf("[%s]\n",s) }
    val shortStack = stack.filter { str => str.contains(".main(") && ! str.contains("$") }.map {
      // derive main class name from stack when main object is declared in source
      _.replaceAll("[.].*","").
      replaceAll("\\s+at\\s+","")
    }
    // for( s <- shortStack ){ System.err.printf("[%s]\n",s) }
    shortStack.take(1).mkString("|")
  }

  lazy val verbose = Option(System.getenv("DOTC_VERBOSE")) match
    case None => false
    case _ => true
}
