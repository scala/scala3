trait RunDSL

val rdsl = new RunDSL {}

given [B] => RunDSL as runNNFExpr = rdsl

given [B] => RunDSL as runNNFImpl {
  //override def runDSL(b: NNF[B]): B = b.terminal
}