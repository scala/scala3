// scalajs: --compliant-semantics

import java.io.IOException
import java.util.concurrent.TimeoutException

object IAE {
  def unapply(e: Exception): Option[String] =
    if (e.isInstanceOf[IllegalArgumentException] && e.getMessage != null) Some(e.getMessage)
    else None
}

object EX extends Exception {
  val msg = "a"
  class InnerException extends Exception(msg)
}

trait ExceptionTrait extends Exception

trait TestTrait {
  type ExceptionType <: Exception

  def traitTest(): Unit = {
    try {
      throw new IOException
    } catch {
      case _: ExceptionType => println("success 9.2")
      case _                => println("failed 9.2")
    }
  }
}

object Test extends TestTrait {
  type ExceptionType = IOException

  def main(args: Array[String]): Unit = {
    var a: Int = 1

    try {
      throw new Exception("abc")
    } catch {
      case _: Exception => println("success 1")
      case _            => println("failed 1")
    }

    try {
      throw new Exception("abc")
    } catch {
      case e: Exception => println("success 2")
      case _            => println("failed 2")
    }

    try {
      throw new Exception("abc")
    } catch {
      case e: Exception if e.getMessage == "abc" => println("success 3")
      case _                                     => println("failed 3")
    }

    try {
      throw new Exception("abc")
    } catch {
      case e: Exception if e.getMessage == "" => println("failed 4")
      case _                                  => println("success 4")
    }

    try {
      throw EX
    } catch {
      case EX => println("success 5")
      case _  => println("failed 5")
    }

    try {
      throw new EX.InnerException
    } catch {
      case _: EX.InnerException => println("success 6")
      case _                    => println("failed 6")
    }

    try {
      throw new NullPointerException
    } catch {
      case _: NullPointerException | _:IOException => println("success 7")
      case _                                       => println("failed 7")
    }

    try {
      throw new ExceptionTrait {}
    } catch {
      case _: ExceptionTrait => println("success 8")
      case _                 => println("failed 8")
    }

    try {
      throw new IOException
    } catch {
      case _: ExceptionType => println("success 9.1")
      case _                => println("failed 9.1")
    }

    traitTest() // test 9.2

    def testThrow(throwIt: => Unit): Unit = {
      try {
        throwIt
      } catch {
        // These cases will be compiled as catch cases
        case e: NullPointerException                 => println("NullPointerException")
        case e: IndexOutOfBoundsException            => println("IndexOutOfBoundsException")
        case _: NoSuchElementException               => println("NoSuchElementException")
        case _: EX.InnerException                    => println("InnerException")
        // All the following will be compiled as a match
        case IAE(msg)                                => println("IllegalArgumentException: " + msg)
        case _: ExceptionTrait                       => println("ExceptionTrait")
        case e: IOException if e.getMessage == null  => println("IOException")
        case _: NullPointerException | _:IOException => println("NullPointerException | IOException")
//        case `a`                                     => println("`a`")
        case EX                                      => println("EX")
        case e: IllegalArgumentException             => println("IllegalArgumentException")
        case _: ClassCastException                   => println("ClassCastException")
      }
    }

    testThrow(throw new IllegalArgumentException("abc"))
    testThrow(throw new IllegalArgumentException())
    testThrow(throw new IOException("abc"))
    testThrow(throw new NoSuchElementException())
    testThrow(throw EX)
    testThrow(throw new EX.InnerException)
    testThrow(throw new NullPointerException())
    testThrow(throw new ExceptionTrait {})
    testThrow(throw a.asInstanceOf[Throwable])
    try {
      testThrow(throw new TimeoutException)
      println("TimeoutException did not escape")
    } catch {
      case _: TimeoutException => println("TimeoutException escaped")
    }
  }

}
