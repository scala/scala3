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

  /** Lorem ipsum
    *
    *
    *
    * Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur scelerisque facilisis sapien a lobortis. Fusce ultricies erat ante, sit amet bibendum orci commodo in. Sed elementum tempus ipsum id sodales. Ut quis nisi vitae turpis lacinia mattis id nec orci. Nullam tincidunt accumsan nisl, ac maximus quam eleifend tincidunt. Nunc ipsum nulla, mattis vitae auctor blandit, euismod sit amet elit. Proin sed porttitor nisi. Curabitur tristique pretium nisi. Vestibulum sagittis condimentum blandit. In ac consequat odio, in fermentum turpis. In hac habitasse platea dictumst.
    * Proin scelerisque est sed magna fermentum, at ullamcorper purus porta. Aliquam posuere arcu elit, molestie fermentum justo malesuada non. In eget massa risus. Proin rutrum maximus arcu, et lacinia est suscipit nec. Morbi varius odio pretium turpis ornare, ut sollicitudin nunc egestas. Aliquam pulvinar massa odio, id tempor purus suscipit id. Nunc imperdiet sapien ligula, ut pretium lacus efficitur sit amet. Sed sed urna sed erat tempus sagittis quis eget elit. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Duis accumsan hendrerit nunc, in sagittis tellus. Vivamus mattis ligula sed dolor lacinia iaculis. Pellentesque vel turpis est. Nam pellentesque diam id arcu pharetra, et consectetur eros facilisis. Aliquam erat volutpat.
    *
    * Phasellus ac quam pretium, convallis dui id, vestibulum nibh. Fusce vulputate interdum ullamcorper. Sed nec erat varius, sagittis ipsum eget, interdum ex. Ut sed leo sit amet ligula ullamcorper facilisis et convallis tellus. Nullam consectetur vitae lorem vel mattis. Suspendisse ultrices ornare leo, ut porttitor est finibus vel. Ut faucibus arcu eget sapien lobortis, a luctus arcu posuere. Vivamus faucibus mauris facilisis enim ornare dapibus.
    *
    * Quisque pharetra et orci non aliquet. Sed urna ipsum, commodo et ultricies sed, volutpat at nunc. Cras non lectus ac mauris lobortis efficitur vel ac ante. Mauris vestibulum risus at mauris pretium, vel iaculis dolor pretium. Nam fringilla fermentum lacus et varius. Nulla pulvinar maximus tortor, et venenatis ipsum luctus id. Integer hendrerit tellus felis, eget hendrerit dolor aliquam sit amet.
    *
    * Aenean elementum risus sed enim egestas, vitae imperdiet urna eleifend. Donec elementum leo neque, eu consequat eros placerat vel. Integer pulvinar sem feugiat, tincidunt erat a, porta nulla. Mauris eu urna egestas, facilisis ex sodales, sollicitudin quam. Integer porta metus et nunc blandit lacinia. Integer posuere mauris et dui ornare, a finibus neque tristique. Cras sit amet lectus nunc. Nam facilisis tincidunt efficitur.
    */
  def loremIpsum[T](a: T): Map[T, T] = ???

  /**
    *  &nbsp;
    * | How to convert ... | to a [[PartialFunction]] | to an optional [[Function]] | to an extractor |
    * | :---:  | ---  | --- | --- |
    * | from a [[PartialFunction]] | [[Predef.identity]] | [[lift]] | [[Predef.identity]] |
    * | from optional [[Function]] | [[Function1.UnliftOps#unlift]] or [[Function.unlift]] | [[Predef.identity]] | [[Function1.UnliftOps#unlift]] |
    * | from an extractor | `{ case extractor(x) => x }` | `extractor.unapply _` | [[Predef.identity]] |
    *  &nbsp;
    *
    * @syntax wiki
    */
  def table(foo: String) = ???

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