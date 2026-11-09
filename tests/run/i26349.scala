
class Counter:
  private var count = 0L
  val next: () => Long = () => { count += 1; count }

@main def Test =
  val c = Counter()
  assert(c.next() == 1)  // 1
  assert(c.next() == 2)  // expected 2; 3.9.0-RC1 prints 1
  assert(c.next() == 3)  // expected 3; 3.9.0-RC1 prints 1
