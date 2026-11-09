//> using options -Yno-suspended-units

// As in tests/neg/no-suspended-units-call, but the macro is expanded during an implicit search,
// which retyped the unpositioned error tree (assertPositioned).
def w = summon[Width[Int]] // error
