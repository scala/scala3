// scalac: -Wvalue-discard -Werror

import scala.util.{Either, Right, Left}

case class Failed(msg: String)

def doSomething(): Either[Failed, Unit] =
  Right(())

def log(): Either[Failed, Unit] =
  Left(Failed("whoops you should have flatMapped me"))

def singleExpr(): Either[Failed, Unit] =
  doSomething().map(_ => log()) // error

def block(): Either[Failed, Unit] = {
  doSomething().map(_ => log()) // error
}
