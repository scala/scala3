import language.experimental.captureChecking
import caps.{Classifier, SharedCapability}

// A fresh sub-classifier of A, introduced after Module 1 was compiled.
trait D extends A, Classifier
class DCap extends D

// The D-classified capability flows into Module 1's `only[A].except[B]`: D is a sibling of
// B, hence disjoint from it. Module 1 was not recompiled, and `only[A].except[B]` never
// collapsed to pure -- the open tree admits the later-defined classifier.
def laterModule(d: DCap^): Unit =
  onlyAButNotB(() => use(d))
