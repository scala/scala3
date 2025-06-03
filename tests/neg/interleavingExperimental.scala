//> using options --source 3.5

def ba[A](x: A)[B](using B): B = summon[B] // error: clauseInterleaving was experimental until 3.6
