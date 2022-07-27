import java.util.function.Consumer;

public class StaticMethods {
  public static void simple() {}
  public static <T> void withTypeParam(Consumer<T> method) {}
}