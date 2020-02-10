import compiletime.summonFrom

inline def a = summonFrom {
  case x => ??? // error
}

inline def b = summonFrom {
  case x@String => ??? // error
}