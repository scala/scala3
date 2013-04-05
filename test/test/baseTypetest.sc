package test

import dotty.tools.dotc._
import core._
import Decorators._
import Types._, Symbols._

object baseTypetest extends DottyTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val int = defn.IntType                          //> int  : dotty.tools.dotc.core.Types.Type = TypeRef(ThisType(module class scal
                                                  //| a),Int)
  int.baseClasses                                 //> res0: List[dotty.tools.dotc.core.Symbols.ClassSymbol] = List(class Int, clas
                                                  //| s AnyVal, class NotNull, class Any)
  defn.StringClass.typeConstructor.baseClasses    //> res1: List[dotty.tools.dotc.core.Symbols.ClassSymbol] = List(class String, c
                                                  //| lass CharSequence, class Comparable, class Serializable, class Object, class
                                                  //|  Any)
  defn.StringClass.typeConstructor.baseType(defn.ObjectClass)
                                                  //> res2: dotty.tools.dotc.core.Types.Type = TypeRef(ThisType(module class lang)
                                                  //| ,Object)
  defn.StringClass.typeConstructor.baseType(defn.AnyClass)
                                                  //> res3: dotty.tools.dotc.core.Types.Type = TypeRef(ThisType(module class scala
                                                  //| ),Any)
  defn.StringClass isSubClass defn.NullClass      //> res4: Boolean = false
  defn.StringClass.typeConstructor.baseType(defn.NullClass)
                                                  //> res5: dotty.tools.dotc.core.Types.Type = NoType
                                                  
}