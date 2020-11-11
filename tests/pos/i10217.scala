trait A
trait B
trait C
trait D
trait E
trait F
trait G
trait H
trait I
trait J
trait K
trait L
trait M
trait N
trait O
trait P
trait Q
trait R
trait S
trait T
trait U
trait V
trait W

class Foo[T]

val f1 = Foo[A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U]
val f2 = Foo[A | (B | (C | (D | (E | (F | (G | (H | (I | (J | (K | (L | (M | (N | (O | (P | (Q | (R | (S | (T | U)))))))))))))))))))]
val f3 = Foo[Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | A]
val f4 = Foo[A | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null | Null]
val f5 = Foo[Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | A)))))))))))))))))))]
val f6 = Foo[A | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | (Null | Null))))))))))))))))))]
