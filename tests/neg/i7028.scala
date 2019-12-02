object Test {
  "abc"
    .foo       // error

  "abc"
    .bar.baz   // error

  "abc"
    .bar       // error
    .baz
}