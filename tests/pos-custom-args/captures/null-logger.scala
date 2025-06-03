import annotation.constructorOnly

class FileSystem extends caps.Capability

class NullLogger(using @constructorOnly fs: FileSystem)

def test2(using fs: FileSystem): NullLogger = NullLogger()

