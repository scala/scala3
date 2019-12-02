object bug2 {
  val id: [U] => U => U = [U] => (none: U) => none
  val id1: [U] => U => U = { [U] => (none: U) => none }
  val id2: [U] => U => U =
    [U] => (none: U) => none
}
