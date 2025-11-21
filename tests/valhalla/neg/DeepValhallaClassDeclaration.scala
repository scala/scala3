import scala.annotation.valhalla

/*  

a class extending DeepValhalla must extend AnyVal and have
the valhalla annotation

*/

class A extends DeepValhalla // error


/*  

A DeepValhalla class must must have a non-DeepValhalla field 

*/

@valhalla
class B extends AnyVal

@valhalla class C(val b: B) extends AnyVal with DeepValhalla // error

class D

@valhalla class E(val d: D) extends AnyVal with DeepValhalla // error
