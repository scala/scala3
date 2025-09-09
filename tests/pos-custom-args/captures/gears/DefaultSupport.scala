package gears.async.default

import gears.async._

given AsyncOperations = JvmAsyncOperations
given VThreadSupport.type = VThreadSupport
given VThreadScheduler.type = VThreadScheduler
