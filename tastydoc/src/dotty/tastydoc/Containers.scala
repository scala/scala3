package dotty.tastydoc

abstract class Container()

case class PackageContainer(sign : String, content: List[Container], userDoc: String) extends Container

case class ImportContainer(sign: String, userDoc: String) extends Container

case class ClassContainer(sign: String, defdef: List[Container], valdef: List[Container], typedef: List[Container], userDoc: String) extends Container //TODO: Add classdef

case class TypeContainer(sign: String, userDoc: String) extends Container

case class DefContainer(sign: String, userDoc: String) extends Container

case class ValContainer(sign: String, userDoc: String) extends Container

case class MissingMatchContainer() extends Container