object App {
  def main(args: Array[String]): Unit = {
    trait ModuleSig {
      type F[_]
      type Type[X] = F[X]

      def subst[F[_[_]]](fa: F[List]): F[Type]
    }
    val Module: ModuleSig = new ModuleSig {
      type F[+A] = List[A]

      def subst[FF[_[_]]](fa: FF[List]): FF[Type] = fa
    }
  }
}
