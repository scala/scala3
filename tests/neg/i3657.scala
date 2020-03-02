object App {
  def main(args: Array[String]): Unit = {
    trait ModuleSig {
      type F[_, _]
      type G[_]
      type H[_]

      trait FooSig {
        type Type = F[G[Int], H[Int]]
        def subst[F[_]](fa: F[Int]): F[Type]
      }

      val Foo: FooSig
    }
    val Module: ModuleSig = new ModuleSig {
      type F[x[_]] = Int  // error

      val Foo: FooSig = new FooSig {
        // type Type = Int
        def subst[F[_]](fa: F[Int]): F[Type] = fa
      }
    }
  }
}