package scala.tasty

package object names {

  object Simple {
    def unapply(arg: Name)(implicit ext: Extractor): Option[String] = ext.unapplySimple(arg)
  }

}
