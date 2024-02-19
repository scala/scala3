type Top = Any
type Bot = Nothing

sealed trait Cov[+A]

final case class Wrap1[B <: Top](first: Cov[B])(using Cov[B] <:< Cov[Bot])
final case class Wrap2[B <: Any](first: Cov[B])(using Cov[B] <:< Cov[Bot])

class Test:
  def t10 = Wrap1/* */(new Cov[Bot] {}) // error: Cannot prove that Cov[Top] <:< Cov[Bot]
  def t11 = Wrap1[Bot](new Cov[Bot] {}) // ok

  def t20 = Wrap2/* */(new Cov[Bot] {}) // ok
  def t21 = Wrap2[Bot](new Cov[Bot] {}) // ok
