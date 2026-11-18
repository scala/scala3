//> using options -source:future-migration

// Under -source:future-migration, a leading infix operator that directly follows
// a block keeps its old meaning, i.e. it applies to the block.

@main def Test =
  val a =
    if true then
      10
    else
      1
    + 100
  assert(a == 10)

  val b =
    if true then {
      10
    } else {
      1
    }
    + 100
  assert(b == 10)
