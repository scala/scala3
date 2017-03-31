interface I<T> {}

interface J<T> extends I<T> {
    default void $init$() {
    };
}

class C<T> implements J<T> {}
