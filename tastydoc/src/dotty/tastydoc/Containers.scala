package dotty.tastydoc

abstract class Container

case class PackageContainer(sign : String, content: List[Container]) extends Container

case class ImportContainer(sign: String) extends Container

case class ClassContainer(sign: String, defdef: List[Container], valdef: List[Container], typedef: List[Container]) extends Container //TODO: Add classdef

case class TypeContainer(sign: String) extends Container

case class DefContainer(sign: String) extends Container

case class ValContainer(sign: String) extends Container

case class MissingMatchContainer() extends Container