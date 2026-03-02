// this file is intended to be ran as an argument to the dotty.tools.scripting.ScriptingDriver class

import java.io.File

// create an empty file
def main(args: Array[String]): Unit =
  val file = File("touchedFile.out")
  file.createNewFile();
