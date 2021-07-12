case class Test1(); case class Test2(); case class Test3();
case class Test4(); case class Test5(); case class Test6();

sealed abstract class DSL {
  def cont [P1 >: this.type <: DSL, P2 <: DSL](continuation: => P2) =
    Continue[P1, P2](() => this, () => continuation)
}
case class Continue [P1 <: DSL, P2 <: DSL](p1: () => P1, p2: () => P2) extends DSL

trait More[-A] {}
case class Out[C <: More[A], A](c: C, v: A) extends DSL
case class Nop() extends DSL

val decision1:Boolean = true;
val decision2:Boolean = false;

type P[
ChanA <: More[Test1|Test2],
ChanB <: More[Test3|Test4],
ChanC <: More[Test5|Test6]] =
 ((Out[ChanA,Test1] Continue ((Out[ChanB,Test3] Continue Nop)|(Out[ChanB,Test4] Continue Nop))) //works if remove first 'Continue Nop'
 | (Out[ChanA,Test2] Continue ((Out[ChanC,Test5] Continue Nop)|(Out[ChanC,Test6] Continue Nop))))


def p( chanA: More[Test1|Test2], chanB: More[Test3|Test4], chanC: More[Test5|Test6])
 :P[chanA.type,chanB.type,chanC.type] ={
      if(decision1){
         Out(chanA,Test1()) cont {
            if(decision2){
               Out(chanB,Test3()) cont Nop() //works if replace with 'Out(chanB,Test3())'
            }
            else{
               Out(chanB,Test4()) cont Nop()
            }
         }
      }
      else{
         Out(chanA,Test2()) cont {
            if(decision2){
               Out(chanC,Test5()) cont Nop()
            }
            else{
               Out(chanC,Test6()) cont Nop()
            }
         }
      }
   }