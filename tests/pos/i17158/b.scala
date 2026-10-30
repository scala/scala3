package bar

import foo.*

val a =
  Array("").foreach {
    case "" => ??? // was an error
  }
