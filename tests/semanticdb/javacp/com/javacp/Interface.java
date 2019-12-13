package com.javacp;

public interface Interface {
  void a();
  default void d() {}
  static void s() {}
}
