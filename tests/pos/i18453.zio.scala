// Minimised from zio's ZLayer ++

// In an attempt to fix i18453
// this would break zio's ZLayer
// in the "would-error" cases
class Cov[+W]:
  def add[X >: W, Y](y: Cov[Y]): Cov[X & Y] = ???
  def pre[Y >: W, X](x: Cov[X]): Cov[X & Y] = ???

class Test:
  def a1[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[A & B & C]       = a.add(b).add(c)
  def a2[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[A with B with C] = a.add(b).add(c) // would-error

  def b1[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[A & (B & C)] = a.add(b).add(c) // would-error (a2)
  def b2[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[(A & B) & C] = a.add(b).add(c)
  def b3[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[A & (B & C)] = a.add(b.add(c))
  def b4[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[(A & B) & C] = a.add(b.add(c))


  def c3[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[A & B & C]       = a.pre(b).pre(c)
  def c4[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[A with B with C] = a.pre(b).pre(c) // would-error

  def d1[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[A & (B & C)] = a.pre(b).pre(c) // would-error (c4)
  def d2[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[(A & B) & C] = a.pre(b).pre(c)
  def d3[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[A & (B & C)] = a.pre(b.pre(c))
  def d4[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[(A & B) & C] = a.pre(b.pre(c))


  def add[X, Y](x: Cov[X], y: Cov[Y]): Cov[X & Y] = ???
  def e1[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[A & (B & C)] = add(add(a, b), c) // alt assoc: ok!
  def e2[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[(A & B) & C] = add(add(a, b), c) // reg assoc: ok
  def e3[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[A & (B & C)] = add(a, add(b, c)) // reg assoc: ok
  def e4[A, B, C](a: Cov[A], b: Cov[B], c: Cov[C]): Cov[(A & B) & C] = add(a, add(b, c)) // alt assoc: ok!
