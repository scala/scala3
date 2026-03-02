// This test checks that symbols in `BodyAnnotation` are not copied in
// `transformAnnot` during `PostTyper`.

package json

trait Reads[A] {
  def reads(a: Any): A
}

object JsMacroImpl {
  inline def reads[A]: Reads[A] =
    new Reads[A] { self =>
      def reads(a: Any) = ???
    }
}
