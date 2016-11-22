package dotty.tools.dotc.core

object nesting {
  class C {
  
    class D {
    	private def x = "D"
    	def show = x
    	class E {
    		println(x)
    	}
    }
    
    val foo: D = {
    	class D extends C.this.D {
    		private def x = "foo.D"
    		class E {
    			println(x)
    		}
      }
      new D
    }
  }
  
  val c = new C                                   //> c  : dotty.tools.dotc.core.nesting.C = dotty.tools.dotc.core.nesting$C@1a84d
                                                  //| a23
  val d = c.foo                                   //> d  : dotty.tools.dotc.core.nesting.c.D = dotty.tools.dotc.core.nesting$C$D$1
                                                  //| @2705d88a
  d.show                                          //> res0: String = foo.D
  
}