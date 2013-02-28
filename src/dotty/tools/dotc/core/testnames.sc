package dotty.tools.dotc.core

object testnames {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import Names._
  
  val n = termName("hello")                       //> n  : dotty.tools.dotc.core.Names.TermName = hello
  val tn = n.toTypeName                           //> tn  : dotty.tools.dotc.core.Names.TypeName = hello
  val ln = n.toLocalName                          //> ln  : dotty.tools.dotc.core.Names.LocalName = hello
  assert(tn.toTermName eq n)
  assert(tn.toLocalName eq ln)
  assert(n.toLocalName eq ln)
  
  n == tn                                         //> res0: Boolean = false
  n == ln                                         //> res1: Boolean = false
  n eq tn                                         //> res2: Boolean = false
  n.hashCode                                      //> res3: Int = 0
  tn.hashCode                                     //> res4: Int = 0
  val foo = encodedTermName("++")                 //> foo  : dotty.tools.dotc.core.Names.TermName = $plus$plus
  foo.hashCode                                    //> res5: Int = 5
  foo.toTypeName.hashCode                         //> res6: Int = -5
  
  val nfoo = n ++ foo                             //> nfoo  : dotty.tools.dotc.core.testnames.n.ThisName = hello$plus$plus
  nfoo contains '$'                               //> res7: Boolean = true
  nfoo.replace('$', '.')                          //> res8: dotty.tools.dotc.core.testnames.nfoo.ThisName = hello.plus.plus
  n == EmptyTermName                              //> res9: Boolean = false
  EmptyTermName.start                             //> res10: Int = -1
}