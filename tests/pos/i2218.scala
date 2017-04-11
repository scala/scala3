trait Rule[In]

class C {
  def ruleWithName[In](f: In => Int): Rule[In] = {
     new DefaultRule(f) {}
  }

  class DefaultRule[In](f: In => Int) extends Rule[In]
}
