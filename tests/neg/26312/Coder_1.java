// Minimized from the `cc.redberry:rings` library

public class Coder_1<
        Element,
        Term extends Coder_1.AMonomial<Term>,
        Poly extends Coder_1.AMultivariatePolynomial<Term, Poly>> {

  public interface AMonomial<T> { }
  public interface AMultivariatePolynomial<T1, T2> { }
  public interface Ring<T> { }
  public interface Tokenizer { }

  Element parse(Tokenizer tokenizer) { return null; }

  private Coder_1(Ring<Element> baseRing) {
  }

  public Coder_1<Element, Term, Poly> clone() { return this; }

  public <Oth> Coder_1<Oth, ?, ?> map(Ring<Oth> ring, java.util.function.Function<Element, Oth> func) {
    Coder_1<Element, Term, Poly> _this = this;
    return new Coder_1<Oth, Term, Poly>(ring) {
      @Override
      public Oth parse(Tokenizer tokenizer) {
        return func.apply(_this.parse(tokenizer));
      }
    };
  }
}
