import scala.reflect.TypeTest

type Matcher[A] = A match { case String => A }

def patternMatch[A](a: Any)(using tt: TypeTest[Any, Matcher[A]]): Option[Matcher[A]] = {
  // type T = RDF.Triple[Rdf]
  a match {
    case res: Matcher[A] => Some(res)
    case _ => None
  }
}

def patternMatchWithAlias[A](a: Any)(using tt: TypeTest[Any, Matcher[A]]): Option[Matcher[A]] = {
  type T = Matcher[A]
  a match {
    case res: T => Some(res)
    case _ => None
  }
}

type S = String
type MS = Matcher[S]

type S2 = MS
type MS2 = Matcher[S2]

type Mstuck = Matcher[Nothing]
