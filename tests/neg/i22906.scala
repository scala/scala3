//> using options -rewrite -indent
//> nominally using scala 3.7.0-RC1
// does not reproduce under "vulpix" test rig, which enforces certain flag sets?
import language.experimental.relaxedLambdaSyntax
def program: Int => Int =
    {`1`: Int  =>  5} // error // error
