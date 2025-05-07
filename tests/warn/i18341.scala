
//> using options -Wunused:params,nowarn

import annotation.*

class B(@nowarn useless: Int)

class C(@nowarn("msg=unused") useless: Int)

class D(useless: Int) // warn

class E(@nowarn useful: Int): // warn
  def e = useful * 10 // 10x useful

class X:
  def extensionInCompanion: String = ???
@nowarn // extensionInCompanion
object X:
  implicit def companionConversion(x: X): B = ???

  extension (x: X) def extensionInCompanion: String = ???
