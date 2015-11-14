interface I<T> {}

interface J<T> extends I<T> {
    default void $init$() {
    };
}

class C<T> extends J<T> {}