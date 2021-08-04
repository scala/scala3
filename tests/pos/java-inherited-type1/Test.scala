object Test {
  val j = new J
  // force completion of these signatures
  j.ident(null);
  j.ident1(null);
  j.select(null);
  j.select1(null);

  val message:TestMessage = null
  val builder:TestMessage.Builder = message.toBuilder
  builder.setName("name")

}
