// `fact`'s recurrence is reflected in its return type, so the solver knows the
// equation `fact(n) == (if n == 0 then 1 else n * fact(n - 1))`. When proving a
// goal that mentions `fact(n)`, the solver rewrites it with this equation *once*
// but does not keep unfolding: the recursive `fact(n - 1)` left behind stays
// folded. So a hand-written one-step expansion checks, but a two-step one does
// not.

def fact(n: Int): {r: Int with r == (if n == 0 then 1 else n * fact(n - 1))} =
  if n == 0 then 1 else n * fact(n - 1)

// One unfold: the body is exactly `fact`'s one-step expansion. Accepted.
def expandOnce(n: Int): {r: Int with r == fact(n)} =
  if n == 0 then 1
  else n * fact(n - 1)

// Two unfolds: the body additionally inlines `fact(n - 1)`'s definition. To see
// that this equals `fact(n)`, the solver would have to unfold `fact` a second
// time (for `fact(n - 1)`), which it does not do. Rejected.
def expandTwice(n: Int): {r: Int with r == fact(n)} =
  if n == 0 then 1
  else n * (if n - 1 == 0 then 1 else (n - 1) * fact(n - 2)) // error

// The same limit on closed terms. Checking `fact(k)` assumes that call's result
// predicate and unfolds the calls appearing in it one more round, but no deeper
// (`resultTypeAssumptions` adds the substituted body without recursing into it).
// `fact(1)` reaches `fact(0) == 1` and closes; `fact(2)` only reaches
// `fact(1) == fact(0)` and stalls before `fact(0) == 1`.
def fact0: {r: Int with r == 1} = fact(0)
def fact1: {r: Int with r == 1} = fact(1)
def fact2: {r: Int with r == 2} = fact(2) // error
