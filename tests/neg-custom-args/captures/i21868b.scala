import language.experimental.captureChecking
import language.experimental.modularity
import caps.*

class IO

class File

trait Abstract:
  type C^
  def f(file: File^{C}): Unit

class Concrete1 extends Abstract:
  type C = {}
  def f(file: File) = ()

class Concrete2(io: IO^) extends Abstract:
  type C = {io}
  def f(file: File^{io}) = ()

class Concrete3(io: IO^) extends Abstract:
  type C = {io}
  def f(file: File) = () // error

trait Abstract2(tracked val io: IO^):
  type C^ <: {io}
  def f(file: File^{C}): Unit

class Concrete4(io: IO^) extends Abstract2(io):
  type C = {}
  def f(file: File) = ()

class Concrete5(io1: IO^, io2: IO^) extends Abstract2(io1):
  type C = {io2} // error
  def f(file: File^{io2}) = ()

trait Abstract3[X^]:
  type C^ <: X
  def f(file: File^{C}): Unit

class Concrete6(io: IO^) extends Abstract3[{io}]:
  type C = {}
  def f(file: File) = ()

class Concrete7(io1: IO^, io2: IO^) extends Abstract3[{io1}]:
  type C = {io2} // error
  def f(file: File^{io2}) = ()

class Concrete8(io1: IO^, io2: IO^) extends Abstract3[{io1}]:
  def f(file: File^{io2}) = () // error