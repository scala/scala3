package x
import dotty.tools.dotc._
import core._
import Contexts._
import Symbols._
import Decorators._

object xplore {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val c = Main.newCompiler                        //> c  : dotty.tools.dotc.Compiler = dotty.tools.dotc.Compiler@4ac9131c
  val base = new ContextBase                      //> base  : dotty.tools.dotc.core.Contexts.ContextBase = dotty.tools.dotc.core.C
                                                  //| ontexts$ContextBase@72c743eb
  implicit val ctx = c.rootContext(base.initialCtx)
                                                  //> ctx  : dotty.tools.dotc.core.Contexts.Context = dotty.tools.dotc.core.Contex
                                                  //| ts$InitialContext@4f88f506
	val strClass = defn.StringClass           //> strClass  : dotty.tools.dotc.core.Symbols.ClassSymbol = class String#207
	strClass.baseClasses                      //> res0: List[dotty.tools.dotc.core.Symbols.ClassSymbol] = List(class String#20
                                                  //| 7, class CharSequence#525, class Comparable#96, class Serializable#3795, cla
                                                  //| ss Object#123, class Any#2112)
  strClass.typeConstructor <:< defn.AnyType       //> res1: Boolean = true
  val predef = defn.PredefModule                  //> predef  : dotty.tools.dotc.core.Symbols.TermSymbol = module Predef#1509
  val strd = predef.info.member("String".toTypeName)
                                                  //> strd  : dotty.tools.dotc.core.Denotations.Denotation = type String
  strd.info                                       //> res2: dotty.tools.dotc.core.Types.Type = TypeAlias(TypeRef(ThisType(module c
                                                  //| lass lang#51),String))
  val strType = strd.symbol.typeConstructor       //> strType  : dotty.tools.dotc.core.Types.TypeRef = TypeRef(ThisType(module cla
                                                  //| ss Predef$#1510),String)
  strType <:< defn.AnyType                        //> res3: Boolean = true
  val prdef = defn.PredefModule                   //> prdef  : dotty.tools.dotc.core.Symbols.TermSymbol = module Predef#1509
  predef.isCompleted                              //> res4: Boolean = true
  predef.info.parents                             //> res5: List[dotty.tools.dotc.core.Types.TypeRef] = List(TypeRef(ThisType(modu
                                                  //| le class scala#29),LowPriorityImplicits), TypeRef(ThisType(module class scal
                                                  //| a#29),DeprecatedPredef))
  predef.info                                     //> res6: dotty.tools.dotc.core.Types.Type = TypeRef(ThisType(module class scala
                                                  //| #29),Predef$)
	predef.info.typeSymbol                    //> res7: dotty.tools.dotc.core.Symbols.Symbol = module class Predef$#1510
	val scala = defn.ScalaPackageClass        //> scala  : dotty.tools.dotc.core.Symbols.ClassSymbol = module class scala#29
	scala.info.decl("Predef$".toTypeName)     //> res8: dotty.tools.dotc.core.Denotations.Denotation = module class Predef$
	predef.info.decls                         //> res9: dotty.tools.dotc.core.Scopes.Scope = Scopes(val <init>#4952, val class
                                                  //| Of#4953, type Class#4954, type String#4957, type Function#4958, type Map#496
                                                  //| 1, type Set#4964, val Map#4966, val Map #4967, val Set#4968, val Set #4969, 
                                                  //| type ClassManifest#4970, type OptManifest#4972, type Manifest#4974, val Clas
                                                  //| sManifest#4976, val ClassManifest #4977, val Manifest#4978, val Manifest #49
                                                  //| 79, val NoManifest#4980, val NoManifest #4981, val manifest#4982, val classM
                                                  //| anifest#4985, val optManifest#4988, val identity#4991, val implicitly#4994, 
                                                  //| val locally#4997, val error#5000, val assert#5002, val assert#5004, val assu
                                                  //| me#5007, val assume#5009, val require#5012, val require#5014, val $qmark$qma
                                                  //| rk$qmark#5017, type Pair#5018, module Pair#5021, type Triple#5033, module Tr
                                                  //| iple#5037, class ArrowAssoc#5052, val ArrowAssoc#5067, class Ensuring#5070, 
                                                  //| val Ensuring#5089, class StringFormat#5092, val StringFormat#5103, class Str
                                                  //| ingAdd#5106, val StringAdd#5117, class RichException#5120, val RichException
                                                  //| #5129, class SeqCharSequence#5131, val SeqCharSequence#5143, class ArrayChar
                                                  //| Sequence#5145, val ArrayCharSequence#5157, val StringCanBuildFrom#5159, val 
                                                  //| StringCanBuildFrom #5160, val augmentString#5161, val unaugmentString#5163, 
                                                  //| val print#5165, val println#5167, val println#5168, val printf#5170, val tup
                                                  //| le2ToZippedOps#5173, val tuple3ToZippedOps#5177, val genericArrayOps#5182, v
                                                  //| al booleanArrayOps#5185, val byteArrayOps#5187, val charArrayOps#5189, val d
                                                  //| oubleArrayOps#5191, val floatArrayOps#5193, val intArrayOps#5195, val longAr
                                                  //| rayOps#5197, val refArrayOps#5199, val shortArrayOps#5202, val unitArrayOps#
                                                  //| 5204, val byte2Byte#5206, val short2Short#5208, val char2Character#5210, val
                                                  //|  int2Integer#5212, val long2Long#5214, val float2Float#5216, val double2Doub
                                                  //| le#5218, val boolean2Boolean#5220, val Byte2byte#5222, val Short2short#5224,
                                                  //|  val Character2char#5226, val Integer2int#5228, val Long2long#5230, val Floa
                                                  //| t2float#5232, val Double2double#5234, val Boolean2boolean#5236, class $less$
                                                  //| colon$less#5238, val singleton_$less$colon$less#5243, val conforms#5244, cla
                                                  //| ss $eq$colon$eq#5246, val singleton_$eq$colon$eq#5251, module $eq$colon$eq#5
                                                  //| 252, class DummyImplicit#5258, module DummyImplicit#5260, module RichExcepti
                                                  //| on#5264, module StringAdd#5274, module StringFormat#5288, module Ensuring#53
                                                  //| 02, module ArrowAssoc#5330)
}