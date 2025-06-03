sealed trait A:
  class B extends A

class Test:
  def t1(a: A): Boolean =
    a match
      case b: A#B => true
