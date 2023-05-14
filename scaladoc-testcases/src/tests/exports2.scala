package tests
package exports2

import exports1._

class B:
  val a: A
   = new A
  export a.{Object => Obj, _}
  export X._
  def obj: Obj.type
   = Obj
