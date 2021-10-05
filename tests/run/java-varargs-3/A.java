class A<S> {
  public void gen(S... args) {
  }

  public <T extends S> void gen2(S... args) {
  }
}
class B<S extends java.io.Serializable> {
  public void gen(S... args) {
  }

  public <T extends S> void gen2(S... args) {
  }
}
