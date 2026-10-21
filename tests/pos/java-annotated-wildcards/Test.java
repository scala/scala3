import java.util.function.Function;

public interface Test {
  Function<@Nullable ? super String, @Nullable ? extends Object> mappingFunction();
}
