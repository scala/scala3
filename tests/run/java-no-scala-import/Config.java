public class Config {
  long longVal;
  Long longObj;
  Integer boxed;

  public long getLongVal() {
    return longVal;
  }

  public void setLongVal(long longVal) {
    this.longVal = longVal;
  }

  public Long getLongObj() {
    return longObj;
  }

  public void setLongObj(Long longObj) {
    this.longObj = longObj;
  }

  public Long longLength(String str) {
    return Long.valueOf(str.length());
  }
}
