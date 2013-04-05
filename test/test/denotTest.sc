package test

import dotty.tools.dotc._
import core._
import Decorators._
import Types._, Symbols._

object denotTest extends DottyTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val str = defn.StringClass.typeConstructor      //> str  : dotty.tools.dotc.core.Types.TypeRef = TypeRef(ThisType(module class l
                                                  //| ang),String)
  val d= str.member("getBytes".toTermName)        //> d  : dotty.tools.dotc.core.Denotations.Denotation = val getBytes <and> val g
                                                  //| etBytes <and> val getBytes <and> val getBytes
  d.alternatives                                  //> res0: List[dotty.tools.dotc.core.Denotations.SingleDenotation] = List(val ge
                                                  //| tBytes, val getBytes, val getBytes, val getBytes)
  d.alternatives.map(_.info)                      //> res1: List[dotty.tools.dotc.core.Types.Type] = List(JavaMethodType(List(), L
                                                  //| ist(), RefinedType(TypeRef(ThisType(module class scala),Array), scala$Array$
                                                  //| $T, TypeAlias(TypeRef(ThisType(module class scala),Byte)) | hash = 777673561
                                                  //| )), JavaMethodType(List(x$0), List(TypeRef(ThisType(module class charset),Ch
                                                  //| arset)), RefinedType(TypeRef(ThisType(module class scala),Array), scala$Arra
                                                  //| y$$T, TypeAlias(TypeRef(ThisType(module class scala),Byte)) | hash = 7776735
                                                  //| 61)), JavaMethodType(List(x$0), List(TypeRef(ThisType(module class lang),Str
                                                  //| ing)), RefinedType(TypeRef(ThisType(module class scala),Array), scala$Array$
                                                  //| $T, TypeAlias(TypeRef(ThisType(module class scala),Byte)) | hash = 777673561
                                                  //| )), JavaMethodType(List(x$0, x$1, x$2, x$3), List(TypeRef(ThisType(module cl
                                                  //| ass scala),Int), TypeRef(ThisType(module class scala),Int), RefinedType(Type
                                                  //| Ref(ThisType(module class scala),Array), scala$Array$$T, TypeAlias(TypeRef(T
                                                  //| hisType(module class scala),Byte)) | hash = 777673561), TypeRef(ThisType(mod
                                                  //| ule class scala),Int)), TypeRef(ThisType(module class scala),Unit)))
  val sm = defn.StringClass.companionModule       //> sm  : dotty.tools.dotc.core.Symbols.Symbol = module String
  val d2 = sm.info.member("valueOf".toTermName)   //> d2  : dotty.tools.dotc.core.Denotations.Denotation = val valueOf <and> val v
                                                  //| alueOf <and> val valueOf <and> val valueOf <and> val valueOf <and> val value
                                                  //| Of <and> val valueOf <and> val valueOf <and> val valueOf
	d2.alternatives.map(_.info)               //> res2: List[dotty.tools.dotc.core.Types.Type] = List(JavaMethodType(List(x$0)
                                                  //| , List(TypeRef(ThisType(module class scala),Double)), TypeRef(ThisType(modul
                                                  //| e class lang),String)), JavaMethodType(List(x$0), List(TypeRef(ThisType(modu
                                                  //| le class scala),Float)), TypeRef(ThisType(module class lang),String)), JavaM
                                                  //| ethodType(List(x$0), List(TypeRef(ThisType(module class scala),Long)), TypeR
                                                  //| ef(ThisType(module class lang),String)), JavaMethodType(List(x$0), List(Type
                                                  //| Ref(ThisType(module class scala),Int)), TypeRef(ThisType(module class lang),
                                                  //| String)), JavaMethodType(List(x$0), List(TypeRef(ThisType(module class scala
                                                  //| ),Char)), TypeRef(ThisType(module class lang),String)), JavaMethodType(List(
                                                  //| x$0), List(TypeRef(ThisType(module class scala),Boolean)), TypeRef(ThisType(
                                                  //| module class lang),String)), JavaMethodType(List(x$0, x$1, x$2), List(Refine
                                                  //| dType(TypeRef(ThisType(module class scala),Array), scala$Array$$T, TypeAlias
                                                  //| (TypeRef(ThisType(module class scala),Char)) | hash = 2075434073), TypeRef(T
                                                  //| hisType(module class scala),Int), TypeRef(ThisType(module class scala),Int))
                                                  //| , TypeRef(ThisType(module class lang),String)), JavaMethodType(List(x$0), Li
                                                  //| st(RefinedType(TypeRef(ThisType(module class scala),Array), scala$Array$$T, 
                                                  //| TypeAlias(TypeRef(ThisType(module class scala),Char)) | hash = 2075434073)),
                                                  //|  TypeRef(ThisType(module class lang),String)), JavaMethodType(List(x$0), Lis
                                                  //| t(TypeRef(ThisType(module class scala),Any)), TypeRef(ThisType(module class 
                                                  //| lang),String)))
}