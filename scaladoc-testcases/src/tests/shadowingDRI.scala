package tests.shadowingDRI

trait A[T]
class B

class S:
  class R:
    def findThisDeclaration = 1

  given R: A[B] with {}
