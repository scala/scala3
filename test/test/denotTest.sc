package test

import dotty.tools.dotc._
import core._
import Decorators._
import Types._, Symbols._

object denotTest extends DottyTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val str = defn.StringClass.typeRef              //> str  : dotty.tools.dotc.core.Types.TypeRef = TypeRef(ThisType(module class l
                                                  //| ang#57),String)
  val d= str.member("getBytes".toTermName)        //> d  : dotty.tools.dotc.core.Denotations.Denotation = val getBytes <and> val g
                                                  //| etBytes <and> val getBytes <and> val getBytes
  d.alternatives                                  //> res0: List[dotty.tools.dotc.core.Denotations.SingleDenotation] = List(val ge
                                                  //| tBytes, val getBytes, val getBytes, val getBytes)
  d.alternatives.map(_.info)                      //> res1: List[dotty.tools.dotc.core.Types.Type] = List(JavaMethodType(List(), L
                                                  //| ist(), RefinedType(TypeRef(ThisType(module class scala#35),Array), scala$Arr
                                                  //| ay$$T, TypeAlias(TypeRef(ThisType(module class scala#35),Byte)))), JavaMetho
                                                  //| dType(List(x$0), List(TypeRef(ThisType(module class charset#5432),Charset)),
                                                  //|  RefinedType(TypeRef(ThisType(module class scala#35),Array), scala$Array$$T,
                                                  //|  TypeAlias(TypeRef(ThisType(module class scala#35),Byte)))), JavaMethodType(
                                                  //| List(x$0), List(TypeRef(ThisType(module class lang#57),String)), RefinedType
                                                  //| (TypeRef(ThisType(module class scala#35),Array), scala$Array$$T, TypeAlias(T
                                                  //| ypeRef(ThisType(module class scala#35),Byte)))), JavaMethodType(List(x$0, x$
                                                  //| 1, x$2, x$3), List(TypeRef(ThisType(module class scala#35),Int), TypeRef(Thi
                                                  //| sType(module class scala#35),Int), RefinedType(TypeRef(ThisType(module class
                                                  //|  scala#35),Array), scala$Array$$T, TypeAlias(TypeRef(ThisType(module class s
                                                  //| cala#35),Byte))), TypeRef(ThisType(module class scala#35),Int)), TypeRef(Thi
                                                  //| sType(module class scala#35),Unit)))
  val sm = defn.StringClass.companionModule       //> sm  : dotty.tools.dotc.core.Symbols.Symbol = module String#214
  val d2 = sm.info.member("valueOf".toTermName)   //> d2  : dotty.tools.dotc.core.Denotations.Denotation = val valueOf <and> val v
                                                  //| alueOf <and> val valueOf <and> val valueOf <and> val valueOf <and> val value
                                                  //| Of <and> val valueOf <and> val valueOf <and> val valueOf
	d2.alternatives.map(_.info)               //> res2: List[dotty.tools.dotc.core.Types.Type] = List(JavaMethodType(List(x$0)
                                                  //| , List(TypeRef(ThisType(module class scala#35),Double)), TypeRef(ThisType(mo
                                                  //| dule class lang#57),String)), JavaMethodType(List(x$0), List(TypeRef(ThisTyp
                                                  //| e(module class scala#35),Float)), TypeRef(ThisType(module class lang#57),Str
                                                  //| ing)), JavaMethodType(List(x$0), List(TypeRef(ThisType(module class scala#35
                                                  //| ),Long)), TypeRef(ThisType(module class lang#57),String)), JavaMethodType(Li
                                                  //| st(x$0), List(TypeRef(ThisType(module class scala#35),Int)), TypeRef(ThisTyp
                                                  //| e(module class lang#57),String)), JavaMethodType(List(x$0), List(TypeRef(Thi
                                                  //| sType(module class scala#35),Char)), TypeRef(ThisType(module class lang#57),
                                                  //| String)), JavaMethodType(List(x$0), List(TypeRef(ThisType(module class scala
                                                  //| #35),Boolean)), TypeRef(ThisType(module class lang#57),String)), JavaMethodT
                                                  //| ype(List(x$0, x$1, x$2), List(RefinedType(TypeRef(ThisType(module class scal
                                                  //| a#35),Array), scala$Array$$T, TypeAlias(TypeRef(ThisType(module class scala#
                                                  //| 35),Char))), TypeRef(ThisType(module class scala#35),Int), TypeRef(ThisType(
                                                  //| module class scala#35),Int)), TypeRef(ThisType(module class lang#57),String)
                                                  //| ), JavaMethodType(List(x$0), List(RefinedType(TypeRef(ThisType(module class 
                                                  //| scala#35),Array), scala$Array$$T, TypeAlias(TypeRef(ThisType(module class sc
                                                  //| ala#35),Char)))), TypeRef(ThisType(module class lang#57),String)), JavaMetho
                                                  //| dType(List(x$0), List(TypeRef(ThisType(module class scala#35),Any)), TypeRef
                                                  //| (ThisType(module class lang#57),String)))
}