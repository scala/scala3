trait Label[A]:
  def apply(v: A): String

def g[A]: Label[A] = _.toString

inline def label[A](x: A, inline l: Label[A]): String = l(x)

def label1[A](v: A) = label(v, g)
def label2[A](l: A) = label(l, g)
