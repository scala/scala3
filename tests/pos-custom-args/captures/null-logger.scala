import annotation.capability
import annotation.constructorOnly

@capability class FileSystem

class NullLogger(using @constructorOnly fs: FileSystem)

def test2(using fs: FileSystem): NullLogger = NullLogger()

