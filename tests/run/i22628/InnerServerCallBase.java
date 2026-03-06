package mylib;

abstract class InnerServerCallBase implements ServerCall {
  @Override
  public void close() {
    System.out.println("InnerServerCallBase.close");
  }
}
