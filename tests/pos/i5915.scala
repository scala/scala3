trait RunDSL

val rdsl = new RunDSL {}

given runNNFExpr[B] as RunDSL = rdsl

given runNNFImpl[B] as RunDSL {
  //override def runDSL(b: NNF[B]): B = b.terminal
}