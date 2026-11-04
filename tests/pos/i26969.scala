package inference

class <[+A, -S]
class Abort[-E]
class Env[+R]
class Result[+E, +A]

object Abort:
    def run[E, S](v: Int < (Abort[E] & S)): Result[E, Int] < S = ???

object Env:
    def use[S](): Int < (Env[String => Int < S] & S)                      = ???
    def run[A, S](v: A < (Env[String => Int < Abort[String]] & S)): A < S = ???

// invariant, only so the inferred type shows up in the error
class Probe[T]
def probe[T](v: T): Probe[T] = ???

object Check:
    val a = probe(Env.run(Abort.run(Env.use())))

    // Abort.run handles Abort[String], so this is Result[String, Int] < Any
    val _ : Probe[Result[String, Int] < Any] = a
