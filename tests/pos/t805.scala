trait MatcherYYY  {
  trait NodeImpl;
  trait Matchable extends NodeImpl {
    protected def doMatch : Unit = {}
  }
}
trait BraceMatcherXXX extends MatcherYYY {
  trait NodeImpl2 extends super.NodeImpl {
    def doMatch  (braces : BracePair) : Unit
  }
  trait BracePair {
    trait BraceImpl extends NodeImpl2 with Matchable {
      override def doMatch : Unit = {
        super.doMatch;
        ();
      }
    }
  }
}
