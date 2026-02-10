package covtest

class AllTreesIgnoredLocally:

  // --- Apply ---
  def coveredApply(x: Int): Int = x + 1
  // $COVERAGE-OFF$
  def ignoredApply(x: Int): Int = x + 1
  // $COVERAGE-ON$

  // --- Literal ---
  def coveredLiteral: Int = 42
  // $COVERAGE-OFF$
  def ignoredLiteral: Int = 42
  // $COVERAGE-ON$

  // --- Select (parameterless method on object) ---
  def coveredSelect(s: String): Int = s.length
  // $COVERAGE-OFF$
  def ignoredSelect(s: String): Int = s.length
  // $COVERAGE-ON$

  // --- Ident (local parameterless def) ---
  def coveredIdent: Int =
    def local: Int = 1
    local
  // $COVERAGE-OFF$
  def ignoredIdent: Int =
    def local: Int = 1
    local
  // $COVERAGE-ON$

  // --- transformBranch (if/else) ---
  def coveredBranch(x: Int): String =
    if x > 0 then "pos"
    else "neg"
  // $COVERAGE-OFF$
  def ignoredBranch(x: Int): String =
    if x > 0 then "pos"
    else "neg"
  // $COVERAGE-ON$

  // --- instrumentBody ---
  def coveredBody: Int = 99
  // $COVERAGE-OFF$
  def ignoredBody: Int = 99
  // $COVERAGE-ON$

  def coveredClosureDef: Int ?=> Int =
    99
  // $COVERAGE-OFF$
  def ignoredClosureDef: Int ?=> Int =
    99
  // $COVERAGE-ON$

  def coveredClosureBody: Int ?=> Int =
    99
  def ignoredClosureBody: Int ?=> Int =
  // $COVERAGE-OFF$
    99
  // $COVERAGE-ON$


// --- instrumentSecondaryCtor ---
class CoveredCtor(val x: Int):
  def this(s: String) =
    this(s.length)

// $COVERAGE-OFF$
class IgnoredCtor(val x: Int):
  def this(s: String) =
    this(s.length)
// $COVERAGE-ON$
