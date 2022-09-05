// scalac: -Wvalue-discard -Werror

import scala.util.{Either, Right, Left}

case class Failed(msg: String)

def firstThing(): Either[Failed, Unit] =
  Right(())

def secondThing(): Either[Failed, Unit] =
  Left(Failed("whoops you should have flatMapped me"))

def singleExpr(): Either[Failed, Unit] =
  firstThing().map(_ => secondThing()) // error

def block(): Either[Failed, Unit] = {
  firstThing().map(_ => secondThing()) // error
}
