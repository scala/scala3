object impl:
  case object UNone

import impl.*

opaque type UOption[+A] = (A | UNone.type) // error: Cyclic Reference involving UOption
