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
  nfoo slice (2, 4)                               //> res11: dotty.tools.dotc.core.testnames.nfoo.ThisName = ll
  nfoo take 3                                     //> res12: dotty.tools.dotc.core.Names.Name = hel
  nfoo drop 3                                     //> res13: dotty.tools.dotc.core.Names.Name = lo$plus$plus
  nfoo.head                                       //> res14: Char = h
  nfoo.tail                                       //> res15: dotty.tools.dotc.core.Names.Name = ello$plus$plus
  nfoo.isTermName                                 //> res16: Boolean = true
  val cs = Array('a', 'b', 'c')                   //> cs  : Array[Char] = Array(a, b, c)
  termName(cs, 0, 2)                              //> res17: dotty.tools.dotc.core.Names.TermName = ab
  termName("abc")                                 //> res18: dotty.tools.dotc.core.Names.TermName = abc
  nfoo.filter(_ >= 'l')                           //> res19: dotty.tools.dotc.core.Names.Name = lloplusplus
  nfoo map (_.toUpper)                            //> res20: dotty.tools.dotc.core.Names.Name = HELLO$PLUS$PLUS

  import Decorators._

  val local = "local".toTermName.toLocalName      //> local  : dotty.tools.dotc.core.Names.LocalName = local
  val local1 = local ++ "!"                       //> local1  : dotty.tools.dotc.core.testnames.local.ThisName = local!
  local1                                          //> res21: dotty.tools.dotc.core.testnames.local.ThisName = local!
  val local2 = "Foo.".toTermName ++: local1       //> local2  : dotty.tools.dotc.core.Names.Name = Foo.local!
  local2                                          //> res22: dotty.tools.dotc.core.Names.Name = Foo.local!
  local1.dropRight(2)                             //> res23: dotty.tools.dotc.core.Names.Name = loca
  local1.fromName("Foo.".toTermName ++ local1)    //> res24: dotty.tools.dotc.core.testnames.local1.ThisName = Foo.local!
}