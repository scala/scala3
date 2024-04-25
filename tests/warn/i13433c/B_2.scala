
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
  println(patternMatch[String]("abc")(using (s: Any) => {
    if s.isInstanceOf[Mstuck] then Some[s.type & Matcher[String]](s.asInstanceOf[s.type & Matcher[String]]) else None})) // warn
}
