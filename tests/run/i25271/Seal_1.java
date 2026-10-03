import java.util.List;

public sealed interface Seal_1<I extends Arg_1<?>> extends Top_1 permits NonSeal_1 {
  void run(I arg);

  default List<String> names(final I arg) {
    return List.of("name");
  }

  default boolean ok(final I arg) {
    return true;
  }
}
