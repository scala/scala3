//> using options -Xmax-fuel:50
// (deliberately low, we want this to fail so we can check the error message)

def step: Tactic[Unit] = new Tactic[Unit] {}

val proof: Int = LemmaMacros.applyTactics { // error
  step; step; step; step; step; step; step; step; step; step; step; step; step; step; step
  step; step; step; step; step; step; step; step; step; step; step; step; step; step; step
  step; step; step; step; step; step; step; step; step; step; step; step; step; step; step
}
