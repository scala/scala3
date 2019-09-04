package dotty.tools.dotc.core

object flagtest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  import Flags._

  val pri = Private                               //> pri  : dotty.tools.dotc.core.Flags.FlagSet = private
  val pro = Protected                             //> pro  : dotty.tools.dotc.core.Flags.FlagSet = protected
  val pripro = pri | pro                          //> pripro  : dotty.tools.dotc.core.Flags.FlagSet = private protected
  pripro is pri                                   //> res0: Boolean = true
  pripro is pro                                   //> res1: Boolean = true
  pripro is Local                                 //> res2: Boolean = false
  val pp = allOf(pri, pro)                        //> pp  : dotty.tools.dotc.core.Flags.FlagSet = private protected
  pripro is pp                                    //> res3: Boolean = true
  pri is pp                                       //> res4: Boolean = false
  pri is pripro                                   //> res5: Boolean = true
  Method                                          //> res6: dotty.tools.dotc.core.Flags.FlagSet = <method>
  Abstract                                        //> res7: dotty.tools.dotc.core.Flags.FlagSet = abstract
  Method == Abstract                              //> res8: Boolean = false
  Method.toCommonFlags                            //> res9: dotty.tools.dotc.core.Flags.FlagSet = <method> abstract
  FromStartFlags                                  //> res10: dotty.tools.dotc.core.Flags.FlagSet = private protected <deferred> <p
                                                  //| aram> <accessor> sealed <local> module <package> <expandedname> <covariant>
                                                  //| <contravariant> <static> <touched> <frozen> <existential>
  AccessFlags <= FromStartFlags                   //> res11: Boolean = true
  FromStartFlags <= AccessFlags                   //> res12: Boolean = false
}