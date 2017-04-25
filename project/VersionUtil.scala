import scala.sys.process.Process

object VersionUtil {
  def executeScript(scriptName: String) = {
    val cmd =
      if (System.getProperty("os.name").toLowerCase.contains("windows"))
        s"cmd.exe /c project\\scripts\\build\\$scriptName.bat -p"
      else s"project/scripts/build/$scriptName"
    Process(cmd).lines.head.trim
  }

  /** Seven letters of the SHA hash is considered enough to uniquely identify a
   *  commit, albeit extremely large projects - such as the Linux kernel - need
   *  more letters to stay unique
   */
  def gitHash = executeScript("get-scala-commit-sha").substring(0, 7)
  def commitDate = executeScript("get-scala-commit-date")
}
