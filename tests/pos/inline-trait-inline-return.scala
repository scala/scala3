//> using options -language:experimental.inlineTraits

inline trait Base:
  def f: Int = return 1

trait Child extends Base