import org.jspecify.annotations.*;
import java.util.List;

public class J {
  public @Nullable String field;
  public @Nullable String get() { return null; }
  public List<@Nullable String> list() { return null; }
  @Override public @NonNull String toString() { return ""; }
}
