package dotty.compiler.internal.core

object flagtest {
  println("Welcome to the Scala worksheet")
  
  import Flags._
  
  val pri = Private
  val pro = Protected
  val pripro = pri | pro
  pripro is pri
  pripro is pro
  pripro is Local
  val pp = allOf(pri, pro)
  pripro is pp
  pri is pp
  pri is pripro
  Method
  Abstract
  Method == Abstract
  Method.toCommonFlags
  FromStartFlags
  AccessFlags <= FromStartFlags
  FromStartFlags <= AccessFlags
}