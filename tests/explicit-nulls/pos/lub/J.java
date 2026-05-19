// Minimized from https://scala3.westeurope.cloudapp.azure.com/dashboard/projects/greenfossil/thorium/builds/HarrisL2%2Fscala3%3Aunsafe-explicit-nulls%3A2026-04-01/logs

import java.util.function.Function;
public class J<T> {

    public <U> U execute(Function<? super T,? extends U> function, T input) {
        return function.apply(input);
    }
}