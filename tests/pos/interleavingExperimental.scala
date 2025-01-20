//> using options --source 3.5

import scala.language.experimental.clauseInterleaving

def ba[A](x: A)[B](using B): B = summon[B]