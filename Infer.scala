object Infer{    
    def r2_[F,G,H](in1: F => (G => H))(in2: F => G )(x: F): H = {
        val t1: G => H = in1(x)
        val t2: G = in2(x)
        val t3: H = t1(t2)
        t3
    }


    given r1[F,G]: ( F => (G => F) ) = x: F => (_ => x)
    given r2[F,G,H]: ( (F => (G => H)) => ((F => G) => (F => H)) ) = r2_
    given r3[F,G](using imp: F => G, t: F): G = imp(t)

    def concl[A]: A => A =
        summon[A => A]

    @main def main = concl[Int]
}