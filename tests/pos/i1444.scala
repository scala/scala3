object Test {

class Cls(implicit x:X)
class ClsImpl extends Cls //this works

trait Tr1(implicit x:X)
class TrtImpl extends Tr1 //Compiler: Error: parameterized trait Tr1 lacks argument list

trait Tr2()(implicit x:X)
class Tr2Impl extends Tr2() //this works

trait X
implicit object AnX extends X
}
