import java.util.concurrent.atomic.AtomicReference;

public class UserDataHolderBase extends AtomicReference<String> {
  @Override
  protected Object clone() {
    return null;
  }
}
