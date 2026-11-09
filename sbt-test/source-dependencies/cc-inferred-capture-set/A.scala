import language.experimental.captureChecking

class IO extends caps.SharedCapability

def mk(io: IO) = () => "constant"
