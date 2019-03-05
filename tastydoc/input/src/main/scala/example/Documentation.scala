package example

import scala.collection._
import scala.deprecated

/** This class is used for testing tasty doc generation
 * @constructor create new object
 * @param c1 class parameter 1
 * @param c2 class parameter 2
 * @tparam T class type parameter
 */
abstract class Documentation[T](c1: String, val c2: List[T]) extends Seq[T] {

  /** Auxiliary constructor
   * @param ac auxiliary parameter
   */
  def this(ac: String) = this(ac, Nil)

  /** Test methods with params
   *
   * @param x parameter 1
   * @param y parameter 2
   *
   * @return something is returned
   */
  def methodsWithParams(x : T, y: Int) : List[Map[Int, T]] = ???

  /** Test value
  */
  val v : Int = ???

  protected def protectedMethod = ???
  private def privateMethod = ???

  protected val protectedVal = ???
  private val privateVal = ???

  def abstractDefinition : Int

  //Define function otherwise class should be abstract
  def apply(idx: Int) = ???
  def iterator = ???
  def length = ???

}