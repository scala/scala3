class T

inline given fail1: T:
  val cs = scala.compiletime.summonAll[EmptyTuple]
inline given fail2: [X] => T:
  val cs = scala.compiletime.summonAll[EmptyTuple]
inline given fail3: () => T:
  val cs = scala.compiletime.summonAll[EmptyTuple]

inline given ok1: T = new T:
  val cs = scala.compiletime.summonAll[EmptyTuple]
inline given ok2: [X] => T = new T:
  val cs = scala.compiletime.summonAll[EmptyTuple]
inline given ok3: () => T = new T:
  val cs = scala.compiletime.summonAll[EmptyTuple]
