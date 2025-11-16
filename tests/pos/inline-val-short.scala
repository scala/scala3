// Example in docs/_docs/reference/metaprogramming/inline.md

trait InlineConstants:
  inline val myShort: Short

object Constants extends InlineConstants:
  inline val myShort/*: Short(4)*/ = 4
