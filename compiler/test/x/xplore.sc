package x
import dotty.tools.dotc._
import core._
import Contexts._
import Symbols._
import Decorators._

object xplore {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val c = Main.newCompiler                        //> c  : dotty.tools.dotc.Compiler = dotty.tools.dotc.Compiler@36ff057f
  val base = new ContextBase                      //> base  : dotty.tools.dotc.core.Contexts.ContextBase = dotty.tools.dotc.core.C
                                                  //| ontexts$ContextBase@2980f96c
  implicit val ctx = c.rootContext(base.initialCtx)
                                                  //> ctx  : dotty.tools.dotc.core.Contexts.Context = dotty.tools.dotc.core.Contex
                                                  //| ts$InitialContext@6aadae91
	val strClass = defn.StringClass           //> strClass  : dotty.tools.dotc.core.Symbols.ClassSymbol = class String#213
	strClass.baseClasses                      //> res0: List[dotty.tools.dotc.core.Symbols.ClassSymbol] = List(class String#21
                                                  //| 3, class CharSequence#531, class Comparable#102, class Serializable#3816, cl
                                                  //| ass Object#129, class Any#2133)
  strClass.typeRef <:< defn.AnyType               //> res1: Boolean = true
  val predef = defn.PredefModule                  //> predef  : dotty.tools.dotc.core.Symbols.TermSymbol = module Predef#1515
  val strd = predef.info.member("String".toTypeName)
                                                  //> strd  : dotty.tools.dotc.core.Denotations.Denotation = type String
  strd.info                                       //> res2: dotty.tools.dotc.core.Types.Type = TypeAlias(TypeRef(ThisType(module c
                                                  //| lass lang#57),String))
  val strType = strd.symbol.typeRef               //> strType  : dotty.tools.dotc.core.Types.TypeRef = TypeRef(ThisType(module cla
                                                  //| ss Predef$#1516),String)
  strType <:< defn.AnyType                        //> res3: Boolean = true
  val prdef = defn.PredefModule                   //> prdef  : dotty.tools.dotc.core.Symbols.TermSymbol = module Predef#1515
  predef.isCompleted                              //> res4: Boolean = true
  predef.info.parents                             //> res5: List[dotty.tools.dotc.core.Types.TypeRef] = List(TypeRef(ThisType(modu
                                                  //| le class scala#35),LowPriorityImplicits), TypeRef(ThisType(module class scal
                                                  //| a#35),DeprecatedPredef))
  predef.info                                     //> res6: dotty.tools.dotc.core.Types.Type = TypeRef(ThisType(module class scala
                                                  //| #35),Predef$)
	predef.info.typeSymbol                    //> res7: dotty.tools.dotc.core.Symbols.Symbol = module class Predef$#1516
	val scala = defn.ScalaPackageClass        //> scala  : dotty.tools.dotc.core.Symbols.ClassSymbol = module class scala#35
	scala.info.decl("Predef$".toTypeName)     //> res8: dotty.tools.dotc.core.Denotations.Denotation = module class Predef$
	predef.info.decls                         //> res9: dotty.tools.dotc.core.Scopes.Scope = Scopes(val <init>#4979, val class
                                                  //| Of#4980, type Class#4981, type String#4984, type Function#4985, type Map#498
                                                  //| 8, type Set#4991, val Map#4993, val Map #4994, val Set#4995, val Set #4996, 
                                                  //| type ClassManifest#4997, type OptManifest#4999, type Manifest#5001, val Clas
                                                  //| sManifest#5003, val ClassManifest #5004, val Manifest#5005, val Manifest #50
                                                  //| 06, val NoManifest#5007, val NoManifest #5008, val manifest#5009, val classM
                                                  //| anifest#5012, val optManifest#5015, val identity#5018, val implicitly#5021, 
                                                  //| val locally#5024, val error#5027, val assert#5029, val assert#5031, val assu
                                                  //| me#5034, val assume#5036, val require#5039, val require#5041, val $qmark$qma
                                                  //| rk$qmark#5044, type Pair#5045, module Pair#5048, type Triple#5060, module Tr
                                                  //| iple#5064, class ArrowAssoc#5079, val ArrowAssoc#5094, class Ensuring#5097, 
                                                  //| val Ensuring#5116, class StringFormat#5119, val StringFormat#5130, class Str
                                                  //| ingAdd#5133, val StringAdd#5144, class RichException#5147, val RichException
                                                  //| #5156, class SeqCharSequence#5158, val SeqCharSequence#5170, class ArrayChar
                                                  //| Sequence#5172, val ArrayCharSequence#5184, val StringCanBuildFrom#5186, val 
                                                  //| StringCanBuildFrom #5187, val augmentString#5188, val unaugmentString#5190, 
                                                  //| val print#5192, val println#5194, val println#5195, val printf#5197, val tup
                                                  //| le2ToZippedOps#5200, val tuple3ToZippedOps#5204, val genericArrayOps#5209, v
                                                  //| al booleanArrayOps#5212, val byteArrayOps#5214, val charArrayOps#5216, val d
                                                  //| oubleArrayOps#5218, val floatArrayOps#5220, val intArrayOps#5222, val longAr
                                                  //| rayOps#5224, val refArrayOps#5226, val shortArrayOps#5229, val unitArrayOps#
                                                  //| 5231, val byte2Byte#5233, val short2Short#5235, val char2Character#5237, val
                                                  //|  int2Integer#5239, val long2Long#5241, val float2Float#5243, val double2Doub
                                                  //| le#5245, val boolean2Boolean#5247, val Byte2byte#5249, val Short2short#5251,
                                                  //|  val Character2char#5253, val Integer2int#5255, val Long2long#5257, val Floa
                                                  //| t2float#5259, val Double2double#5261, val Boolean2boolean#5263, class $less$
                                                  //| colon$less#5265, val singleton_$less$colon$less#5270, val conforms#5271, cla
                                                  //| ss $eq$colon$eq#5273, val singleton_$eq$colon$eq#5278, module $eq$colon$eq#5
                                                  //| 279, class DummyImplicit#5285, module DummyImplicit#5287, module RichExcepti
                                                  //| on#5291, module StringAdd#5301, module StringFormat#5315, module Ensuring#53
                                                  //| 29, module ArrowAssoc#5357)
}