import scala.annotation.experimental

@experimental
class A

@experimental
class B extends A

@experimental
type X

@experimental
type Y = Int

@experimental
opaque type Z = Int

type AA = A // error
type BB = Z // error
type XX = Z // error
type YY = Z // error
type ZZ = Z // error
