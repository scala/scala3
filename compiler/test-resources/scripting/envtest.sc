// this file is intended to be ran as an argument to the dotty.tools.scripting.ScriptingDriver class

def main(args: Array[String]): Unit =
  println("Hello " + util.Properties.propOrNull("key"))
