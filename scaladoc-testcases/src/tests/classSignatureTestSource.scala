package tests.classSignatureTestSource

import scala.collection.*
import scala.deprecated
import scala.annotation.*
import scala.math.{Pi, max}
import example.level2.Documentation

abstract class Documentation[T, A <: Int, B >: String, -X, +Y](c1: String, val c2: List[T]) extends Seq[T], Product, Serializable
{
    def this(ac: String)
    = this(ac, Nil)

    def this()
    = this("", Nil)

    def this(x: T)
    = this()

    //expected: def toArray[B >: T : ClassTag]: Array[B]

    class innerDocumentationClass
    {

    }

    sealed trait CaseImplementThis(id: Int)

    case class IAmACaseClass(x: T, id: Int) extends CaseImplementThis(id) //expected: case class IAmACaseClass(x: Documentation.this.T, id: Int) extends CaseImplementThis

    case class IAmACaseClassWithParam[T](x: Documentation.this.T, id: T)

    case object IAmACaseObject extends CaseImplementThis/*<-*/(0)/*->*/

    object testObject

    class Graph
    {
        type Node = Int
    }

    type typeExample[X] >: X <: String

    type abstractType
}

object Documentation
{
  // TODO We do not see members from companions val valInsideDocObject = ???
}

sealed abstract class ClassExtendingDocumentation[T, A <: Int, B >: String, -X, +Y] extends Documentation[T, A, B, X, Y]
{}

trait TraitTest
{

}

trait TraitWithCompanion{} //expected: trait TraitWithCompanion

object TraitWithCompanion
{}

// TODO #25 do we need to add 'val' in case class signatures?
case class ManyModifiers(/*<-*/val /*->*/x: Int, var y: Double, z: String)
class ManyModifiers2(val x: Int, var y: Double, z: String)