trait Rule[-In, +A] extends (In => A) {
  def flatMap[In2 <: In, B](fa2ruleb: A => Rule[In2, Seq[B]]): Rule[In, Seq[B]] = ???
  def map[B](fa2b: A => B): Rule[In, B] = ???

  def ~++[In2, B >: A](next: => Rule[In2, Seq[B]]) = for (a <- this; b <- next) yield a :: b.toList
  // def ~++[In2, B >: A](next: => Rule[In2, Seq[B]]): Rule[In, Seq[B]] = for (a <- this; b <- next) yield a :: b.toList
}

class SeqRule {
  type S
  type A
  def * : Rule[S, List[A]] = ???
  def +(rule: Rule[S, A]) : Rule[S, Seq[A]] = rule ~++ *
}
