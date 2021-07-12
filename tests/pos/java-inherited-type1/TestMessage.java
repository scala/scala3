abstract class AbstractMessage  {
  public static abstract class Builder<BuilderType extends Builder<BuilderType>> {
  }
}

class TestMessage extends AbstractMessage {

  public Builder toBuilder() {
    return null;
  }

  public static class Builder extends AbstractMessage.Builder<Builder> {
    public Builder setName(String name) {
      return this;
    }
  }
}
