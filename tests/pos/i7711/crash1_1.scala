package scalaLibV

import reflect.Selectable.reflectiveSelectable

// Church-encoded Booleans, in DOT.
type IFT = (x : { type IFTA }) => x.IFTA => x.IFTA => x.IFTA

val iftTrue : IFT =
  x => t => f => t
