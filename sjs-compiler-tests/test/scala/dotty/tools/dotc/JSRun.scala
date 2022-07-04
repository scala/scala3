package dotty.tools.dotc

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import dotty.tools.vulpix.*

import org.scalajs.jsenv.*
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.logging.*

object JSRun:
  def runJSCode(sjsCode: Path)(using ExecutionContext): Status =
    val logger = new ScalaConsoleLogger(Level.Warn)

    var stdoutStream: Option[InputStream] = None
    var stderrStream: Option[InputStream] = None

    val input = Input.Script(sjsCode) :: Nil
    val config = RunConfig()
      .withLogger(logger)
      .withInheritOut(false)
      .withInheritErr(false)
      .withOnOutputStream { (out, err) =>
        stdoutStream = out
        stderrStream = err
      }

    val run = new NodeJSEnv().start(input, config)
    try
      val success = try {
        Await.result(run.future, Duration.Inf)
        true
      } catch {
        case _: Exception =>
          false
      }
      val output = readStreamFully(stderrStream) + readStreamFully(stdoutStream)
      if success then
        Success(output)
      else
        Failure(output)
    finally
      run.close()
  end runJSCode

  private def readStreamFully(optStream: Option[InputStream]): String =
    optStream match
      case None =>
        ""
      case Some(stream) =>
        try
          val result = new java.io.ByteArrayOutputStream()
          val buffer = new Array[Byte](1024)
          while ({
            val len = stream.read(buffer)
            len >= 0 && {
              result.write(buffer, 0, len)
              true
            }
          }) ()
          new String(result.toByteArray(), StandardCharsets.UTF_8)
        finally
          stream.close()
  end readStreamFully
end JSRun
