// this file is intended to be ran as an argument to the dotty.tools.scripting.ScriptingDriver class

  def main(args: Array[String]): Unit =
    val name = Option(sys.props("script.name")) match {
    case None => printf("no script.name property is defined\n")
    case Some(name) =>
      printf("script.name: %s\n",name)
      assert(name == "scriptName.scala")
    }
