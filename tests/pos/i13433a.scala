//> using options -Xfatal-warnings -deprecation -feature

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

@main def main = {
  println(patternMatch[String]("abc"))
  println(patternMatchWithAlias[String]("abc"))
  println(patternMatch[String]("abc")(using (s: Any) => {
    if s.isInstanceOf[Matcher[String]] then Some[s.type & Matcher[String]](s.asInstanceOf[s.type & Matcher[String]]) else None }))
  println(patternMatchWithAlias[String]("abc")(using (s: Any) => {
    if s.isInstanceOf[Matcher[String]] then Some[s.type & Matcher[String]](s.asInstanceOf[s.type & Matcher[String]]) else None }))

  println(patternMatch[String](1))
  println(patternMatchWithAlias[String](1))

  println(patternMatch[String]("abc")(using (s: Any) => {
    if s.isInstanceOf[S] then Some[s.type & Matcher[String]](s.asInstanceOf[s.type & Matcher[String]]) else None}))
  println(patternMatch[String]("abc")(using (s: Any) => {
    if s.isInstanceOf[MS] then Some[s.type & Matcher[String]](s.asInstanceOf[s.type & Matcher[String]]) else None}))
  println(patternMatch[String]("abc")(using (s: Any) => {
    if s.isInstanceOf[S2] then Some[s.type & Matcher[String]](s.asInstanceOf[s.type & Matcher[String]]) else None}))
  println(patternMatch[String]("abc")(using (s: Any) => {
    if s.isInstanceOf[MS2] then Some[s.type & Matcher[String]](s.asInstanceOf[s.type & Matcher[String]]) else None}))
}
