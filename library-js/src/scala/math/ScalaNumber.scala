/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.math

import scala.language.`2.13`

/** A marker class for Number types introduced by Scala
 *  @author  Martin Odersky, Paul Phillips
 *  @version 2.8
 *  @since 2.8
 */
abstract class ScalaNumber extends java.lang.Number {
  protected def isWhole(): Boolean
  def underlying(): Object
}
