#!/usr/bin/env scala

  def main(args: Array[String]): Unit =
    args.zipWithIndex.foreach { case (arg,i) => printf("arg %d: [%s]\n",i,arg) }
    val path = Option(sys.props("script.path")) match {
    case None => printf("no script.path property is defined\n")
    case Some(path) =>
      printf("script.path: %s\n",path)
      assert(path.endsWith("scriptPath.sc"),s"actual path [$path]")
    }
