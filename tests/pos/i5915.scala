trait RunDSL

val rdsl = new RunDSL {}

given RunNNFExpr[B] as RunDSL = rdsl

given RunNNFImpl[B] as RunDSL {
  //override def runDSL(b: NNF[B]): B = b.terminal
}