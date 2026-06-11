public class Coder_1<
        Element,
        Term extends Coder_1.AMonomial<Term>,
        Poly extends Coder_1.AMultivariatePolynomial<Term, Poly>>
        /*implements IParser<Element>, IStringifier<Element>, java.io.Serializable*/ {

  public interface AMonomial<T> { }
  public interface AMultivariatePolynomial<T1, T2> { }
  public interface Ring<T> { }
  public interface Tokenizer { }

  Element parse(Tokenizer tokenizer) { return null; }

  private Coder_1(Ring<Element> baseRing/*,
                  Map<String, Element> eVariables,
                  MultivariateRing<Poly> polyRing,
                  Map<String, Integer> pVariables,
                  SerializableFunction<Poly, Element> polyToElement*/) {
  }

  public Coder_1<Element, Term, Poly> clone() { return this; }

  public <Oth> Coder_1<Oth, ?, ?> map(Ring<Oth> ring, java.util.function.Function<Element, Oth> func) {
    Coder_1<Element, Term, Poly> _this = this;
    return new Coder_1<Oth, Term, Poly>(ring/*, _eVariables, null, null, null*/) {
/*      {
        this.subcoders.putAll(_this.subcoders);
        this.bindings.putAll(_bindings);
      }*/
      @Override
      public Oth parse(Tokenizer tokenizer) {
        return func.apply(_this.parse(tokenizer));
      }
    };
  }
}
