trait MyData

object MyData:
  extension (m: MyData)
    def printIt() = println("hey from my data")

class MyClass:
  def sel(s: String): Int = s.hashCode()

enum MyTag[A]:
  case MyDataTag  extends MyTag[MyData]
  case MyClassTag extends MyTag[MyClass]

def callExtension[A](tag: MyTag[A], a:A): Unit =
  tag match
    case MyTag.MyDataTag  => a.printIt()
    case MyTag.MyClassTag => a.sel("hi")

def callExtensionDirectly(m: MyData): Unit =
  m.printIt()
