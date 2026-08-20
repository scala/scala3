class C { def setA(a: Int): this.type = this }
def addMode(c: C): c.type = c
val x = addMode(identity(new C()).setA(???)).setA(???)
