class C:
  def addMode(c: C): C =
    println("memberAddMode")
    this

  def bar(): C =
    println("bar")
    this

class D:
  def bar(): this.type =
    println("dep.bar")
    this

def foo(): C =
  println("foo")
  C()

def bar(): C =
  println("argBar")
  C()

def addMode(c: C): C =
  println("addMode")
  c

def depFoo(): D =
  println("depFoo")
  D()

def depAddMode(d: D): d.type =
  println("depAddMode")
  d

@main
def Test: Unit =
  addMode(foo())
  addMode(foo()).bar()
  addMode(foo()).addMode(bar()).bar()
  depAddMode(depFoo()).bar()
