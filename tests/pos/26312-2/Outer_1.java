
class Outer_1<A, B> {
    public <Oth> Outer_1<Oth, Oth> make() {
        return new Outer_1<Oth, Oth>() { };
    }
}
