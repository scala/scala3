import java.nio.file.Paths

object ScriptParent {
  def main(args: Array[String]): Unit = {
    args.zipWithIndex.foreach { case (arg,i) => printf("arg %d: [%s]\n",i,arg) }
    val scriptName = Option(sys.props("script.path")) match {
      case  None =>
        printf("no script.path property\n")
      case Some(script) =>
        val p = Paths.get(script).toAbsolutePath.toFile.getParent
        printf("parentDir: [%s]\n",p)
    }
  }
}
