import util.Try
import caps.Control

trait NotControl extends caps.Classifier, caps.SharedCapability

class A extends NotControl:
  def hi = println("hi")

class B extends Control:
  def hi = println("hi")

def a[T](f: A => T): T = f(A())
def b[T](f: B => T): T = f(B())

def try_a(): Try[Unit] =
  a: a =>
    Try(a.hi)

def try_b(): Try[Unit] =
  b: b => // error
    Try(b.hi)

