package com.javacp;

public enum Coin {
  PENNY(1), NICKEL(5), DIME(10), QUARTER(25);
  Coin(int value) { this.value = value; }

  private final int value;
  public int value() { return value; }
}
