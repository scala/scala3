import caps.{cap, consume, Mutable}
import language.experimental.captureChecking

class Buffer extends Mutable

def f1(@consume buf: Buffer^): Buffer^ =
  val buf1: Buffer^ = buf // OK
  buf1

def f2(@consume buf: Buffer^): Buffer^ =
  def g(): Buffer^ = buf // error
  g()

def f3(@consume buf: Buffer^): Buffer^ =
  val buf1 = buf
  def g(): Buffer^ = buf1 // error
  g()

def f4(@consume buf: Buffer^): Buffer^ =
  val buf1: Buffer^ = buf
  def g(): Buffer^ = buf1 // error
  g()

