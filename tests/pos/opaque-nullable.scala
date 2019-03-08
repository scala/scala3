object nullable {
  opaque type Nullable[A >: Null <: AnyRef] = A

  object Nullable {
    def apply[A >: Null <: AnyRef](a: A): Nullable[A] = a

    implicit class NullableOps[A >: Null <: AnyRef](na: Nullable[A]) {
      def exists(p: A => Boolean): Boolean =
        na != null && p(na)
      def filter(p: A => Boolean): Nullable[A] =
        if (na != null && p(na)) na else null
      def flatMap[B >: Null <: AnyRef](f: A => Nullable[B]): Nullable[B] =
        if (na == null) null else f(na)
      def forall(p: A => Boolean): Boolean =
        na == null || p(na)
      def getOrElse(a: => A): A =
        if (na == null) a else na
      def map[B >: Null <: AnyRef](f: A => B): Nullable[B] =
        if (na == null) null else f(na)
      def orElse(na2: => Nullable[A]): Nullable[A] =
        if (na == null) na2 else na
      def toOption: Option[A] =
        Option(na)
    }
  }
}