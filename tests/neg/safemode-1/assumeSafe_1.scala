package test
import language.experimental.captureChecking
import caps.{assumeSafe, rejectSafe}

@assumeSafe object A:
  def foo() = println("ok")

@assumeSafe class A:
  def foo() = println("ok")
  def bam() = println("ok")

class Unsafe:
  def foo() = println("ok")
  @rejectSafe val a: A = A()

object Unsafe:
  def foo() = println("ok")
