import java.io.{File, BufferedReader, InputStreamReader}
import scala.util.Properties.isWin
import java.lang.ProcessBuilder.Redirect
import sbt.MessageOnlyException

object Process {

  /** Prepare command to be passed to ProcessBuilder */
  def prepareCommand(cmd: Seq[String]): Seq[String] =
    if (isWin) Seq("cmd.exe", "/C") ++ cmd
    else cmd

  def runProcess(cmd: Seq[String], wait: Boolean = false, directory: Option[File] = None, outputCallback: Option[BufferedReader => Unit] = None): Unit = {
    val pb = new ProcessBuilder(prepareCommand(cmd): _*)

    directory match {
      case Some(dir) =>
        pb.directory(dir)
      case None =>
    }

    pb.redirectInput(Redirect.INHERIT)
      .redirectError(Redirect.INHERIT)
      .redirectOutput(
        outputCallback match {
          case Some(_) =>
            Redirect.PIPE
          case None =>
            Redirect.INHERIT
        })

    val process = pb.start()
    outputCallback match {
      case Some(callback) =>
        callback(new BufferedReader(new InputStreamReader(process.getInputStream)))
      case None =>
    }
    if (wait) {
      val exitCode = process.waitFor()
      if (exitCode != 0) {
        val cmdString = cmd.mkString(" ")
        val description = if (directory != null) s""" in directory "$directory"""" else ""
        throw new MessageOnlyException(s"""Running command "${cmdString}"${description} failed.""")
      }
    }
  }

}
