package dotty.tools.dotc.callbacks;

import java.io.File;

/**
 *  NOTE: This interface is used for integration of Dotty compiler with Intellij IDEA.
 *  If you want to change it please contact Scala Plugin team.
 */

public interface Position
{
	File sourceFile();

	String sourcePath();

	int line();

	String lineContent();

	int offset();

	int column();
}