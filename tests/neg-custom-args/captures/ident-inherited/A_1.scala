package test

import caps.*

object Consol extends SharedCapability:
  def println(s: String) = ()

trait A uses Consol:
  def println(): Unit = Consol.println("hello")
  def print: Unit = println()

