public interface Built_1 {
    interface Builder<A> extends GeneralBuilder<Built_1, Builder<A>> {}
}

interface GeneralBuilder<R, B extends GeneralBuilder<R, B>> {}
