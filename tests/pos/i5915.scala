trait RunDSL

val rdsl = new RunDSL {}

given RunNNFExpr[B]: RunDSL = rdsl

given RunNNFImpl[B]: RunDSL with {
  //override def runDSL(b: NNF[B]): B = b.terminal
}