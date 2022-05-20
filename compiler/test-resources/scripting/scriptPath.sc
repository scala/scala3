#!/usr/bin/env dist/target/pack/bin/scala

  def main(args: Array[String]): Unit =
    args.zipWithIndex.foreach { case (arg,i) => printf("arg %d: [%s]\n",i,arg) }

    Option(sys.props("script.path")) match {
    case Some(path) =>
      if ! path.endsWith("scriptPath.sc") then
        printf( s"incorrect script.path defined as [$path]")
      else
        printf("script.path: %s\n",path) // report the value
    case None =>
      printf("no script.path property is defined\n")
      // report relevant environment factors that might explain the error
      val psep: String = Option(sys.props("path.separator")).get
      val pathEntries = System.getenv("PATH").split(psep).toList
      System.err.printf("sun.java.command: %s\n", sys.props("sun.java.command"))
      System.err.printf("first 5 PATH entries:\n%s\n",pathEntries.take(5).mkString("\n"))
    }

  extension(s: String)
    def norm: String = s.replace('\\', '/')
