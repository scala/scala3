// scalajs: --skip

public class Test {
	public static void main(String[] args) {
		C<String> c = new C<String>();
		// TODO add a bridge during mixin so we can expose
		// sharper generic signature for `tail`.
		/*Traversable<String>*/ Object ls = c.tail();
	}
}
