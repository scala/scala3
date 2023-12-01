//> using options -Yimports:hello.world.minidef

import hello.{world => hw}
import hw.minidef.{Magic => Answer}

// Finds the answer, but dumb to forget Numb
class C {
  val v: Numb = Answer // error
  def greet() = println("hello, world!") // error
}
