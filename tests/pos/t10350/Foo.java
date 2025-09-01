
package foo;

public interface Foo {
  static Foo create(java.lang.String v) {
    return null;
  }
}

/*
5 |  static Foo create(java.lang.String v) {
  |                    ^^^^^^^^^
  |                    value lang is not a member of foo.java
 */
