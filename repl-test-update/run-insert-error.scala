def replTestDir = "../compiler/test-resources"

def list(dir: String = "."): Vector[String] =
    Option(java.io.File(dir).list).map(_.toVector).getOrElse(Vector())

def loadLines(fileName: String, enc: String = "UTF-8"): Vector[String] = 
  var result = Vector.empty[String]
  val source = scala.io.Source.fromFile(fileName, enc)
  try result = source.getLines().toVector finally source.close()
  result

def saveString(text: String, fileName: String, enc: String = "UTF-8"): Unit = 
  val f = java.io.File(fileName)
  val pw = java.io.PrintWriter(f, enc)
  try pw.write(text) finally pw.close()

extension (s: String) def isDir: Boolean = java.io.File(s).isDirectory


def visitFile(path: String, isModify: Boolean): Unit =
  print(s"\nprocessing: $path")
  val lines = loadLines(path)
  var result = Vector[String]() 
  for i <- lines.indices do
    result :+= lines(i)
    if lines(i).startsWith("scala>") && 
      lines.lift(i + 1).map(_.startsWith("1 | ")).getOrElse(false) 
    then
      result :+= "-- Error:"

  val (msg, isDiff) = 
    if lines == result then (Console.GREEN + " unmodified" + Console.RESET, false)
    else (Console.RED + " *** MODIFIED" + Console.RESET, true)
  println(msg)
  if isDiff then 
    println("\nbefore ==========>\n" + lines.mkString("\n"))
    println("\nafter  ==========>\n" + result.mkString("\n"))
    if isModify then 
      println(s"Writing: $path")
      saveString(result.mkString("\n"), path)

def visitDirRecursively(dirName: String, isModify: Boolean): Unit =
  println(s"Enter directory: $dirName")
  list(dirName).foreach( f =>
    val path = s"$dirName/$f"
    if path.isDir then visitDirRecursively(path, isModify)
    else visitFile(path, isModify)
  )

@main 
def run_insert_error(isModify: Boolean): Unit = 
  visitDirRecursively(replTestDir, isModify)
