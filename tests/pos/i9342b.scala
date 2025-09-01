trait Label[A]:
  def apply(v: A): String

given [A] => Label[A] = _.toString

extension [A](x: A) inline def label(using inline l: Label[A]): String = l(x)

def label1[A](v: A) = v.label

def label2[A](l: A) = l.label
