package example;

public abstract class Builder<T extends Builder<T>> {
  public abstract String build();
  public abstract T addService(Service service);
}
