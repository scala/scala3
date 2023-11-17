package dotty.tools.dotc
package core.tasty

import scala.language.unsafeNulls

import dotty.tools.tasty.{TastyFormat, TastyReader, TastyBuffer}

import java.nio.charset.StandardCharsets

class AttributeUnpickler(reader: TastyReader):
  import reader._

  lazy val attributeTags: List[Int] =
    val listBuilder = List.newBuilder[Int]
    while !isAtEnd do listBuilder += readNat()
    listBuilder.result()

  lazy val attributes: Attributes = {
    var scala2StandardLibrary = false
    var explicitNulls = false
    for attributeTag <- attributeTags do
      attributeTag match
        case TastyFormat.SCALA2STANDARDLIBRARYattr => scala2StandardLibrary = true
        case TastyFormat.EXPLICITNULLSattr => explicitNulls = true
        case attribute =>
          assert(false, "Unexpected attribute value: " + attribute)
    Attributes(
      scala2StandardLibrary,
      explicitNulls,
    )
  }

end AttributeUnpickler
