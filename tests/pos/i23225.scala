import language.experimental.captureChecking

class A

def run(f: PartialFunction[A, A]^): A = f(A())

class File extends caps.SharedCapability:
  def read() = ()

def main() =
  val file = File()

  run:
    case a =>
      file.read()
      a
