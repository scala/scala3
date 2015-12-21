package dotty.tools.dotc.callbacks;

import java.io.File;

public interface Position
{
	File sourceFile();

	String sourcePath();

	int line();

	String lineContent();

	int offset();

	int column();
}