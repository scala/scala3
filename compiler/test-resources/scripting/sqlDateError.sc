// this file is intended to be ran as an argument to the dotty.tools.scripting.ScriptingDriver class

def main(args: Array[String]): Unit = {
  println(new java.sql.Date(100L))
  System.err.println("SCALA_OPTS="+Option(System.getenv("SCALA_OPTS")).getOrElse(""))
}
