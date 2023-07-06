package dotty.tools.pc.tests.signaturehelp

import dotty.tools.pc.base.BaseSignatureHelpSuite

import org.junit.Test

class SignatureHelpHKSuite extends BaseSignatureHelpSuite:

  @Test def `op` =
    check(
      """object A:
        |  trait Test[F[_]]:
        |    def op[A, B](fa: F[A])(f: (A) => B): F[B] = ???
        |  val x: Test[Option] = ???
        |  x.op(a @@)
        |""".stripMargin,
      """|op[A, B](fa: Option[A])(f: A => B): Option[B]
         |         ^^^^^^^^^^^^^
         |""".stripMargin
    )
