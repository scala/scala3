package test

import dotty.tools.dotc._
import core._
import Decorators._
import Types._, Symbols._

object sigtest extends DottyTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val int = requiredClass("scala.Int")            //> int  : dotty.tools.dotc.core.Symbols.ClassSymbol = class Int
  int.signature                                   //> res0: dotty.tools.dotc.core.Denotations.Signature = List()
  val intmeth = methType("x")(int.symbolicRef)()  //> intmeth  : dotty.tools.dotc.core.Types.MethodType = MethodType(List(x), List
                                                  //| (TypeRef(ThisType(module class scala),Int)), TypeRef(ThisType(module class s
                                                  //| cala),Unit))
  intmeth.signature                               //> res1: dotty.tools.dotc.core.Denotations.Signature = List(Int)
  val arr = defn.ArrayType.appliedTo(int.symbolicRef)
                                                  //> arr  : dotty.tools.dotc.core.Types.Type = RefinedType(TypeRef(ThisType(modul
                                                  //| e class scala),Array), scala$Array$$T, TypeAlias(TypeRef(ThisType(module cla
                                                  //| ss scala),Int)) | hash = 1907214242)
  val arraymeth = methType("x")(arr)()            //> arraymeth  : dotty.tools.dotc.core.Types.MethodType = MethodType(List(x), Li
                                                  //| st(RefinedType(TypeRef(ThisType(module class scala),Array), scala$Array$$T,
                                                  //| TypeAlias(TypeRef(ThisType(module class scala),Int)) | hash = 1907214242)),
                                                  //| TypeRef(ThisType(module class scala),Unit))
  arraymeth.signature                             //> res2: dotty.tools.dotc.core.Denotations.Signature = List(Int[])
  val curriedmeth = methType("x", "y")(defn.IntType, defn.BooleanType)(methType("z")(defn.ArrayType.appliedTo(defn.IntType))())
                                                  //> curriedmeth  : dotty.tools.dotc.core.Types.MethodType = MethodType(List(x, y
                                                  //| ), List(TypeRef(ThisType(module class scala),Int), TypeRef(ThisType(module c
                                                  //| lass scala),Boolean)), MethodType(List(z), List(RefinedType(TypeRef(ThisType
                                                  //| (module class scala),Array), scala$Array$$T, TypeAlias(TypeRef(ThisType(modu
                                                  //| le class scala),Int)) | hash = 1808140099)), TypeRef(ThisType(module class s
                                                  //| cala),Unit)))
  curriedmeth.signature                           //> res3: dotty.tools.dotc.core.Denotations.Signature = List(Int, Boolean, Int[]
                                                  //| )
}