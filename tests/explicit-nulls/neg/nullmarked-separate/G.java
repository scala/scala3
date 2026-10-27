package unmarked;

import org.jspecify.annotations.*;
import java.util.List;
import java.util.Map;

@NullMarked
public class G<T extends @Nullable Object, U> {
  public T get() { return null; }
  public @Nullable U getNullable() { return null; }
  public void set(T t, U u) {}
  public <V> V id(V v) { return v; }
  public List<String> list() { return null; }
  public List<@Nullable String> nullableList() { return null; }
  public List<? extends @Nullable CharSequence> wildcard() { return null; }
  public List<?> unboundedWildcard() { return null; }
  public Map<String, @Nullable List<String>> nested() { return null; }
}
