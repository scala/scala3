enum Bool with
  case True
  case False

import Bool._

enum SBool[B <: Bool] with
  case STrue extends SBool[True.type]
  case SFalse extends SBool[False.type]

import SBool._

def f(b: SBool[True.type]): Unit = b match
  case STrue => ()
