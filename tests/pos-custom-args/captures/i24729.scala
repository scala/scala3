import scala.collection.SeqOps

inline def foo[A, B](self: A, y: B): (A, B) = (self, y)
def bar[A, B](self: A, y: B): (A, B) = (self, y)

object Test:
  def test[A, CC[_] <: Seq[?], C <: SeqOps[A, CC, C]](t: (C & SeqOps[A, CC, C])^) =
    foo(t.head, t.tail)
    bar(t.head, t.tail)

extension [A](self: A) inline def --> [B](y: B): (A, B) = (self, y)
extension [A](inline self: A) inline def ---> [B](inline y: B): (A, B) = (self, y)

object +: {
  def unapply[A, CC[_] <: Seq[?], C <: SeqOps[A, CC, C]](t: (C & SeqOps[A, CC, C])^): Option[(A, C^{t})] =
    if(t.isEmpty) None
    else Some(t.head --> t.tail)
  def unapply2[A, CC[_] <: Seq[?], C <: SeqOps[A, CC, C]](t: (C & SeqOps[A, CC, C])^): Option[(A, C^{t})] =
    if(t.isEmpty) None
    else Some(t.head ---> t.tail)
}
