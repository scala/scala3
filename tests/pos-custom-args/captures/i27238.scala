import language.experimental.captureChecking
import language.experimental.separationChecking
import scala.caps.{any, Control}

object Test:
  def fork(consume t: ->{any.except[Control]} Unit): Unit = ???

  fork:
    var x = 10

  def forkOnly(consume t: ->{any.only[Control]} Unit): Unit = ???

  forkOnly:
    val x = 10
