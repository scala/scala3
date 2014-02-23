package test

import dotty.compiler.internal._
import core._
import Decorators._
import Types._, Symbols._

object baseTypetest extends DottyTest {
  println("Welcome to the Scala worksheet")
  val int = defn.IntType
  int.baseClasses
  defn.StringClass.typeRef.baseClasses
  defn.StringClass.typeRef baseType(defn.ObjectClass)
  defn.StringClass.typeRef.baseType(defn.AnyClass)
  defn.StringClass isSubClass defn.NullClass
  defn.StringClass.typeRef.baseType(defn.NullClass)
                                                  
}