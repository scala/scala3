abstract class C { val x: Int }
class D extends C { def x = 1 } // error: method x of type => Int needs to be a stable, immutable value
