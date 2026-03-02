import framework.*

def getRandom: Int = brokenRandom // LINE 3;

def brokenRandom: Int = ??? // LINE 5;

@entrypoint
def run = println("Hello, here is a random number: " + getRandom) // LINE 8;
