case class A()
case class B()

type C = A ?=> B
def m(): C = ???

def test = m() // error
