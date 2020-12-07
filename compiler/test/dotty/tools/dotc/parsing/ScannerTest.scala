package dotty.tools
package dotc
package parsing

import dotty.tools.io._
import scala.io.Codec
import util._
import Tokens._, Scanners._
import reporting.Diagnostic

trait ScannerTest extends DottyTest:
  import ParserTest._

  def scan(name: String): Unit = scan(new PlainFile(File(name)))

  def scan(file: PlainFile): Unit = scanSourceEither(new SourceFile(file, Codec.UTF8)).toTry.get

  def reset() = resetCtx()

  private def scanSourceEither(source: SourceFile): Either[ParserError, Unit] =
    //println("***** scanning " + file)
    val scanner = new Scanner(source)
    var i = 0
    while scanner.token != EOF do
//    print("[" + scanner.token.show +"]")
      scanner.nextToken()
//      i += 1
//      if (i % 10 == 0) println()

    if getCtx.reporter.hasErrors || getCtx.reporter.hasWarnings then
      val result = Left(ParserError(getCtx.reporter.allErrors))
      reset()
      result
    else Right(())

  def scanTextEither(code: String): Either[ParserError, Unit] = scanSourceEither(SourceFile.virtual("<code>", code))
  def scanText(code: String): Unit = scanTextEither(code).toTry.get
