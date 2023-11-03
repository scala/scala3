package dotty.tools.dotc.util

import dotty.tools.dotc.parsing.*
import Scanners.*
import Tokens.*

object kwords {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  keywords.toList.map(tokenString)                //> res0: List[String] = List(if, for, else, this, null, new, with, super, case,
                                                  //|  case class, case object, val, abstract, final, private, protected, override
                                                  //| , implicit, var, def, type, extends, true, false, object, class, import, pac
                                                  //| kage, yield, do, trait, sealed, throw, try, catch, finally, while, return, m
                                                  //| atch, lazy, then, forSome, _, :, =, <-, =>, ';', ';', <:, >:, #, @, <%)
  keywords.toList.filter(kw => tokenString(kw) == null)
                                                  //> res1: List[Int] = List()
  canStartStatTokens3 contains CASE               //> res2: Boolean = false

}