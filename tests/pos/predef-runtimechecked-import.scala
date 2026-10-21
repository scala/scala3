package example

import scala.Predef.{assert => _, *}

class PredefRuntimeCheckedImport:
  def f: Any = {
    for (_ <- 1 to 1) ()
    Map(1.runtimeChecked -> "")
  }
