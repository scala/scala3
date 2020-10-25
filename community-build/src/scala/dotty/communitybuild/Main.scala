package dotty.communitybuild

object Main {
  /** Builds stdlib.
   *
   *  Output is available in build/pack/lib directory in stdlib project.
   *
   *  In the future, we allow building different projects based on arguments,
   *  but for now stdlib is the only usecase.
   */
  def main(args: Array[String]): Unit =
    if args.length != 2 then
      println("USAGE: <COMMAND> <PROJECT NAME>")
      println("COMMAND is one of: publish doc")
      println("Available projects are:")
      projects.projectMap.keys.foreach { k =>
        println(s"\t$k")
      }
      sys.exit(0)

    val Array(cmd, proj) = args
    cmd match {
      case "doc" => projects(proj).doc()
      case "publish" => projects(proj).publish()
    }
}
