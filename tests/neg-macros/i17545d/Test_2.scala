def testB =
  case object WhateverB
  val whateverB: WhateverB.type = InvokeConstructor[WhateverB.type] // error
