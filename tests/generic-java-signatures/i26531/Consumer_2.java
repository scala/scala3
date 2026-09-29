package i26531;

public final class Consumer_2 {
    interface Command {}
    static final class Create implements Command {}

    public static void test() {
        Builder<Command> builder = new Builder<>();
        builder.accept(Create.class, (Create command) -> {});
    }
}
