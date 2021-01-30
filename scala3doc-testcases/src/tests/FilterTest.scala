package tests

trait FilterTestBaseTrait:
   /** doc */
  protected def protectetDefInheriteTrait(a: Int): String = ???
  /** doc */
  private def privateDefInheritedTrait(a: Int): String = ???
  /** doc */
  def publicDefInheritedTrait(a: Int): String = ???

  /** doc */
  object PublicObjectInheritedTrait
  /** doc */
  protected object ProtectedObjectInheritedTrait

  /** doc */
  protected val protectetValInheritedTrait = 123
  /** doc */
  private val privateValInheritedTrait = 344
  /** doc */
  val publicValInheritedTrait = 567

class FilterTestBase:
  /** doc */
  sealed abstract class BInherited
  /** doc */
  abstract case class CInherited(s: String)
  /** doc */
  sealed case class DInherited(c: String)
  /** doc */
  final case class EInherited(c: String)
  /** doc */
  private class PrivateInherited
  /** doc */
  protected class ProtectedInherited
  /** doc */
  protected def protectetDefInherited(a: Int): String = ???
  /** doc */
  private def privateDefInherited(a: Int): String = ???
  /** doc */
  def publicDefInherited(a: Int): String = ???

  /** doc */
  object PublicObjectInherited
  /** doc */
  protected object ProtectedObjectInherited

  /** doc */
  protected val protectetValInherited = 123
  /** doc */
  private val privateValInherited = 344
  /** doc */
  val publicValInherited = 567

  /** doc */
  protected type protectedTypeInherited = 123
  /** doc */
  private type privateTypeInherited = 344
  /** doc */
  type publicTypeInherited = 567

  /** doc */
  protected given Set[String | Int] = Set(1, "ala")
  /** doc */
  given Map[String, Double] = Map.empty

  /** doc */
  protected given namedSet: Set[String | Int] = Set(1, "ala")
  /** doc */
  given namedMap: Map[String, Double] = Map.empty

class FilterTest extends FilterTestBase with FilterTestBaseTrait:
  /** doc */
  sealed abstract class B
  /** doc */
  abstract case class C(s: String)
  /** doc */
  sealed case class D(c: String)
  /** doc */
  final case class E(c: String)
  /** doc */
  private class Private
  /** doc */
  protected class Protected

  /** doc */
  object PublicObject
  /** doc */
  protected object ProtectedObject

  /** doc */
  protected def protectetDef(a: B): String = ???
  /** doc */
  private def privateDef(a: C): String = ???
  /** doc */
  def publicDef(a: D): FilterTest = ???


  /** doc */
  protected val protectetVal = 123
  /** doc */
  private val privateVal= 344
  /** doc */
  val publicVal = 567

  /** doc */
  protected type protectedType = 123
  /** doc */
  private type privateType= 344
  /** doc */
  type publicType = 567

  /** doc */
  protected given Seq[String | Int | Double] = List(1)
  /** doc */
  given List[String] = "ula" :: Nil

  /** doc */
  given namedList: List[String] = "ula" :: Nil
  /** doc */
  protected given namedSeq: Seq[String | Int | Double] = List(1)

extension (e: FilterTest)
  def extensionMethod(name: FilterTest): FilterTest = ???

extension (e: FilterTestBase)
  def extensionMethodBase(name: FilterTest): FilterTest = ???