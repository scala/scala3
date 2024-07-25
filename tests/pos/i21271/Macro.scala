import scala.quoted.*

trait Schema
object Schema:
  lazy val sampleDate: String = "" // lazy val requried to reproduce

  inline def derived: Schema =
    annotations
    new Schema {}

inline def annotations: Int = ${ annotationsImpl }
def annotationsImpl(using Quotes): Expr[Int] = Expr(1)
