package scala.util

import scala.language.experimental.saferExceptions

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import TestUtil.*

class UsingTest:
  @Test def getTest: Unit =
    assertCompile("Try(1).get", "The capability to throw exception Exception is missing.")
    try Try(2).get
    catch case e: Exception => ()
    Try(2).get(using unsafeExceptions.canThrowAny)
    { import unsafeExceptions.given; Try(3).get }
    { import unsafeExceptions.canThrowAny; Try(3).get }
