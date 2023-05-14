class A
class B extends A
type AA = A { type T = Int }
type BA = B { type T = Int }
type AL = A { type T >: Int }
type BL = B { type T >: Int }
type AU = A { type T <: Int }
type BU = B { type T <: Int }

def aa(x: AA) = x.isInstanceOf[BA] // was: the type test for BA cannot be checked at runtime
def al(x: AL) = x.isInstanceOf[BL] // was: the type test for BL cannot be checked at runtime
def au(x: AU) = x.isInstanceOf[BU] // was: the type test for BU cannot be checked at runtime

// an alias leaves nothing unchecked when type testing against one bound:
def bl(x: AA) = x.isInstanceOf[BL] // was: the type test for BL cannot be checked at runtime
def bu(x: AA) = x.isInstanceOf[BU] // was: the type test for BU cannot be checked at runtime

// but static knowledge of only one bound makes checking against an alias unchecked:
def al_ba(x: AL) = x.isInstanceOf[BA] // error: the type test for BA cannot be checked at runtime
def au_ba(x: AU) = x.isInstanceOf[BA] // error: the type test for BA cannot be checked at runtime
def al_bu(x: AL) = x.isInstanceOf[BU] // error: the type test for BU cannot be checked at runtime
def au_bl(x: AU) = x.isInstanceOf[BL] // error: the type test for BL cannot be checked at runtime
