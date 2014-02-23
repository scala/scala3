package dotty.compiler.internal.core

object testnames {
  println("Welcome to the Scala worksheet")
  
  import Names._
  val n = termName("hello")
  val tn = n.toTypeName
  val ln = n.toLocalName // fails to compile
  assert(tn.toTermName eq n)
  assert(tn.toLocalName eq ln) // fails to compile
  assert(n.toLocalName eq ln) // fails to compile
  
  n == tn
  n == ln
  n eq tn
  n.hashCode
  tn.hashCode
  val foo = encodedTermName("++") // fails to compile
  foo.hashCode
  foo.toTypeName.hashCode
  
  val nfoo = n ++ foo
  nfoo contains '$'
  nfoo.replace('$', '.')
  n == EmptyTermName
  EmptyTermName.start
  nfoo slice (2, 4)
  nfoo take 3
  nfoo drop 3
  nfoo.head
  nfoo.tail
  nfoo.isTermName
  val cs = Array('a', 'b', 'c')
  termName(cs, 0, 2)
  termName("abc")
  nfoo.filter(_ >= 'l')
  nfoo map (_.toUpper)
  
  import Decorators._

  val local = "local".toTermName.toLocalName // fails to compile
  val local1 = local ++ "!"
  local1
  val local2 = "Foo.".toTermName ++: local1
  local2
  local1.dropRight(2)
  local1.fromName("Foo.".toTermName ++ local1)
}