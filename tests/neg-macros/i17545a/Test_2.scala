case object WhateverA

def testA =
  val whateverA: WhateverA.type = InvokeConstructor[WhateverA.type] // error
