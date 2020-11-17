package dotty.communitybuild

object Main {
  /** Allows running various commands on community build projects. */
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
