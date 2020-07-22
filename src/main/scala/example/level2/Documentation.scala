package example
/** Test
*/
package level2

import scala.collection._
import scala.deprecated
import scala.annotation._
import scala.math.{Pi, max}

/** This class is used for testing tasty doc generation
 * @constructor create new object
 * @author Bryan Abate
 * @param c1 class parameter 1
 * @param c2 class parameter 2
 * @tparam T class type parameter
 */
@strictfp
abstract class Documentation[T, A <: Int, B >: String, -X, +Y](c1: String, val c2: List[T]) extends Seq[T] with Product with Serializable{

  /** Auxiliary constructor
   * @param ac auxiliary parameter
   */
  def this(ac: String) = this(ac, Nil)

  def this() = this("", Nil)

  def this(x: T) = this()

  class innerDocumentationClass {

  }

  sealed trait CaseImplementThis(id: Int)
  case class IAmACaseClass(x: T, id: Int) extends CaseImplementThis(id)
  case object IAmACaseObject extends CaseImplementThis(0)

  object testObject {

  }

  def defReturningInnerClass(): innerDocumentationClass = ???

  /** Test methods with params
   *
   * @param x parameter 1
   * @param y parameter 2
   *
   * @return something is returned
   */
  def methodsWithParams(x : T, y: Int) : List[Map[Int, T]] = ???

  def methodsWithImplicit(x: Int)(implicit imp: Int, notImp: String) = ???

  def methodsWithCallByName(x: => Int) = ???

  def methodsWithDefault(x: Int = 42) = ???

  class Graph {
    type Node = Int
  }
  def linkingGraph(g: Graph): g.Node = ???

  val refinementTest:
    Graph {
      //def x(a: String, b: Double)(c: Float): Int
      def x: Int
      def x2: innerDocumentationClass
      type Y = String
      val z: Boolean
    }

  /** Test value
  */
  @showAsInfix
  val v : Int = ???

  protected def protectedMethod = ???
  private def privateMethod = ???

  protected val protectedVal = ???
  private val privateVal = ???

  def abstractDefinition : Int

  def apply(idx: Int) = ???
  def iterator = ???
  override def length = ???

  /** method: [[example.UserDocLinkingClass.linkMeFromUserDoc]]
   *
   * method:[[example.level2.Documentation.apply]]
   *
   * class: [[example.UserDocLinkingClass]]
   */
  def linkMethodInDoc() = ???

  /** An example documention with markdown formatting
   *
   *  **I'm bold**
   *
   *  *I'm italic*
   *
   *  `some code`
   *  ```scala
   *  def someScalaCode(x: String) = println("Hello " + x)
   *  ```
   *
   *# Title of level 1
   *# Title of level 1
   *
   *  1. I'm a list
   *
   *
   *  * Multilevel List
   *             1. level 2
   *    1. level 2 2
   *  * level 1 again
   *
   *  * multilevel try2
   *    * try2 level2
   */
  def docWithMd = ???

  def functionWithType[U >: String]() : U

  val complexTypeVal : Int | List[List[T]] & String | (Double | Int, Double) | ((Int) => (String))

  type typeExample[X] >: X <: String //TypeBound

  type abstractType

  def useOfOutsideType(): ReturnTypeClass[T] = ???
  def useOfOutsideTypeInsideObject(): ReturnObjectWithType.returnType = ???
  def useOfSameLevelOutsideType(): SameLevelTypeLinking = ???

  protected[example] val valWithScopeModifier = ???
  protected[this] val valWithScopeModifierThis = ???

  var iAmAVar = ???
}

/** Companion object
 */
object Documentation {
  val valInsideDocObject = ???
}

sealed abstract class ClassExtendingDocumentation[T, A <: Int, B >: String, -X, +Y] extends Documentation[T, A, B, X, Y] {}

trait TraitTest {

}

val valueInAPackage = 0

def defInAPackage(abc: String): List[Int] = ???

trait TraitWithCompanion{}

object TraitWithCompanion{}