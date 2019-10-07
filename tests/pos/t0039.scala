abstract class Extensible[A, Self <: Extensible[A, Self]](x: A, xs: Self) { self: Self =>
    def mkObj(x: A, xs: Self): Self;
}
class Fixed[A](x: A, xs: Fixed[A]) extends Extensible[A, Fixed[A]](x, xs) {
    def mkObj(x: A, xs: Fixed[A]) = new Fixed(x, xs);
}
