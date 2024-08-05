//> using options -source 3.7

object Minimization:

  trait A
  trait B extends A

  def test1(using a1: A)(using    b1: B) = summon[A] // picks (most general) a1
  def test2(using a2: A)(implicit b2: B) = summon[A] // error: ambiguous
  def test3(implicit       a3: A, b3: B) = summon[A] // picks (most specific) b3

end Minimization
