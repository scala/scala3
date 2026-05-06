
interface I {
    default int f() { return 42; }
}

public class Over implements I {
    public int f(int i) { return 42 + i; }
}
