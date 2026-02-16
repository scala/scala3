//> using options -Wunused:all

import Test.OnlyFirst
import Test.OnlyThird

enum Test:
  case First extends Test with OnlyFirst
  case Second extends Test
  case Third(s: String) extends Test with OnlyThird

object Test:
  sealed trait OnlyFirst
  sealed trait OnlyThird
