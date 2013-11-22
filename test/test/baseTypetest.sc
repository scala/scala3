package test

import dotty.tools.dotc._
import core._
import Decorators._
import Types._, Symbols._

object baseTypetest extends DottyTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val int = defn.IntType                          //> int  : dotty.tools.dotc.core.Types.Type = TypeRef(ThisType(module class scal
                                                  //| a#35),Int)
  int.baseClasses                                 //> res0: List[dotty.tools.dotc.core.Symbols.ClassSymbol] = List(class Int#1364,
                                                  //|  class AnyVal#575, class Any#2133)
  defn.StringClass.typeRef.baseClasses            //> res1: List[dotty.tools.dotc.core.Symbols.ClassSymbol] = List(class String#21
                                                  //| 3, class CharSequence#531, class Comparable#102, class Serializable#3816, cl
                                                  //| ass Object#129, class Any#2133)
  defn.StringClass.typeRef.baseType(defn.ObjectClass)
                                                  //> res2: dotty.tools.dotc.core.Types.Type = TypeRef(ThisType(module class lang#
                                                  //| 57),Object)
  defn.StringClass.typeRef.baseType(defn.AnyClass)//> res3: dotty.tools.dotc.core.Types.Type = TypeRef(ThisType(module class scala
                                                  //| #35),Any)
  defn.StringClass isSubClass defn.NullClass      //> res4: Boolean = false
  defn.StringClass.typeRef.baseType(defn.NullClass)
                                                  //> res5: dotty.tools.dotc.core.Types.Type = NoType
                                                  
}