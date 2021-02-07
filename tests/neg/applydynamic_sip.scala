import scala.language.dynamics
object Test extends App {
  val qual: Dynamic = ???
  val expr = "expr"
  val a = "a"
  val a2 = "a2"

  qual.sel(a, a2*) // error
  qual.sel(arg = a, a2*) // error
  qual.sel(arg, arg2 = "a2", a2*) // error

  class Bad1 extends Dynamic {
    def selectDynamic(n: Int) = n
    def applyDynamic(n: Int) = n
    def applyDynamicNamed(n: Int) = n
    def updateDynamic(n: Int) = n

  }
  val bad1 = new Bad1
  bad1.sel // error
  bad1.sel(1) // error
  bad1.sel(a = 1) // error
  bad1.sel = 1 // error

  class Bad2 extends Dynamic {
    def selectDynamic = 1
    def applyDynamic = 1
    def applyDynamicNamed = 1
    def updateDynamic = 1
  }
  val bad2 = new Bad2
  bad2.sel // error
  bad2.sel(1) // error
  bad2.sel(a = 1) // error
  bad2.sel = 1 // error
}
