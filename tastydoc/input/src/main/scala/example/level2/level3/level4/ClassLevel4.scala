package example.level2.level3.level4

import example.level2.Documentation

sealed abstract class ClassLevel4[T, A <: Int, B >: String, -X, +Y]() extends Documentation[T, A, B, X, Y] {

  /**
   * [[example.level2.Documentation]]
   * [[example.level2.Documentation$.valInsideDocObject]]
   * [[example.level2.Documentation.abstractType]]
   */
  def linkingToDocMethodInUserDoc = ???
}