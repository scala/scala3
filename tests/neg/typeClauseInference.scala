import scala.language.experimental.typeClauseInference

val notInScopeInferred: [T] => T => T = x => (x: T) // error
