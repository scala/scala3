//> using options -language:experimental.specializedTraits

inline trait Vec[T: Specialized](val xs: List[T]):
    inline def zip[S: Specialized](other: Vec[S]): Vec[(T, S)] = 
        new Vec[(T, S)](xs.zip(other.xs)) {}

@main def Test =
    val xs = new Vec[Double](List(9.1, 68.52, 18.4, 83.5)) {}
    val ys = new Vec[String](List("Switzerland", "France", "The Netherlands", "Germany")) {}
    val zs = new Vec[String](List("Bern", "Paris", "The Hague", "Berlin")) {}

    xs.zip(ys).zip(zs).xs.map( (numberCountry, capital) =>
        val (number, country) = numberCountry
        println(s"The population of ${country} is ${number} million and its capital is ${capital}")
    )
