package scala.util

import scala.compiletime.testing.{typeChecks, typeCheckErrors}

object TestUtil:
  inline def assertCompile(inline code: String, msgPrefix: String): Unit =
    typeCheckErrors(code).headOption.map(_.message) match
      case Some(msg) =>
        if !msg.startsWith(msgPrefix) then
          throw new AssertionError(
            s"""Expected error message starting with: $msgPrefix
              |got '$msg'
              |Code: $code
              |""".stripMargin
          )
      case _ =>
        throw new AssertionError(s"Expected error, got none. Code: $code")
