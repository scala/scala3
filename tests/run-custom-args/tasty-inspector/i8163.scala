import scala.quoted._
import scala.tasty.inspector._

opaque type PhoneNumber = String

case class I8163() {
  val phone: PhoneNumber = "555-555-5555".asInstanceOf[PhoneNumber]
  val other: String = "not a phone"
}

object Test {
  def main(args: Array[String]): Unit = {
    new TestInspector().inspect("", List("I8163"))
  }
}

class TestInspector() extends TastyInspector:

  protected def processCompilationUnit(using QuoteContext)(root: qctx.reflect.Tree): Unit =
    inspectClass(root)

  private def inspectClass(using QuoteContext)(tree: qctx.reflect.Tree): Unit =
    import qctx.reflect._
    tree match {
      case t: PackageClause =>
        t.stats.map( m => inspectClass(m) )
      case t: ClassDef if !t.name.endsWith("$") =>
        val interestingVals = t.body.collect {
          case v: ValDef => v
        }
        val shouldBePhone = interestingVals.find(_.name == "phone").get
        val shouldBePhoneType = shouldBePhone.tpt.tpe match {
          case tr: TypeRef => tr
          case _ => throw new Exception("unexpected")
        }
        assert(shouldBePhoneType.isOpaqueAlias)
        assert(shouldBePhoneType.translucentSuperType.show == "scala.Predef.String")

        val shouldNotBePhone = interestingVals.find(_.name == "other").get
        val shouldNotBePhoneType = shouldNotBePhone.tpt.tpe match {
          case tr: TypeRef => tr
          case _ => throw new Exception("unexpected")
        }
        assert(!shouldNotBePhoneType.isOpaqueAlias)

      case x =>
    }