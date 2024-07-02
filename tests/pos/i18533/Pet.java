//> using javacOpt --enable-preview --source 17
//> test: -jvm 17+

package i18533;

public sealed class Pet permits Cat, Dog {

}