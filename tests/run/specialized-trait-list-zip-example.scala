//> using options -language:experimental.specializedTraits -Werror -Vprint:erasure
//> using scala 3.10.0-RC1-bin-SNAPSHOT-nonbootstrapped

import scala.annotation.nowarn

@nowarn("id=E233")
sealed inline trait List[+T: Specialized]:
    inline def zip[S: Specialized](other: List[S]): List[(T, S)] = 
        def zip(xxs: List[T], yys: List[S]): List[(T, S)] = (xxs, yys) match {
            case (_: Nill[_], _) => Nill()
            case (_, _: Nill[_]) => Nill()
            case (xxs: :+:[T @unchecked], yys: :+:[S @unchecked]) =>  :+:((xxs.head, yys.head), zip(xxs.tail, yys.tail))  // : (
        }
        zip(this, other)
    
    def foreach[S](f: T => Unit): Unit = (this: List[T]) match { // : (
        case xs: :+:[T @unchecked] => f(xs.head); xs.tail.foreach(f)  // : (
        case _: Nill[_] => 
    }


sealed inline trait Nill[T: Specialized] extends List[T]
sealed inline trait :+:[T: Specialized](val head: T, val tail: List[T]) extends List[T]

object Nill {
    inline def apply[T: Specialized]() = new Nill[T]() {}
}

object :+: {
    inline def apply[T: Specialized](head: T, tail: List[T]): List[T] =
        new :+:[T](head, tail) {}
}

object List:
    inline def apply[T: Specialized](values: T*) = 
        values.foldRight[List[T]](Nill())(:+:.apply)

@main def Test =
    val xs: List[Double] = :+:(9.1, :+:(68.52, :+:(18.4, :+:(83.5, Nill[Double]())))) // <- : (
    val ys = List("Switzerland", "France", "The Netherlands", "Germany")
    val zs = List("Bern", "Paris", "Berlin", "The Hague")

    xs.zip(ys).zip(zs).foreach( (numberCountry, capital) =>
            val (number, country) = numberCountry
            println(s"The population of ${country} is ${number} million and its capital is ${capital}")
        )
