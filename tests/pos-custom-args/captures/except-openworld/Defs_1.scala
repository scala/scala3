import language.experimental.captureChecking
import caps.{any, Classifier, SharedCapability}

// Open-world classifiers and separate compilation.
//
// Exclusion systems based on a closed-world assumption (e.g. some boolean-algebra effect
// systems) need to know all members of a classifier to form its complement. Excluding
// every *known* sub-classifier of A would make `only[A].except[<known children>]` collapse
// to the empty set, so a body bounded by it would suddenly "appear pure" -- and a
// sub-classifier introduced by another module later would silently break that reasoning.
//
// The classifier tree is open: every node is assumed to have infinitely many children, so
// excluding the only currently-known child B does NOT empty `only[A].except[B]`. Module 1
// need not know every sub-classifier of A, which keeps separate compilation sound.
trait A extends SharedCapability, Classifier
trait B extends A, Classifier

def onlyAButNotB[T](body: () ->{any.only[A].except[B]} T): T = body()
def use(x: Any): Unit = ()
