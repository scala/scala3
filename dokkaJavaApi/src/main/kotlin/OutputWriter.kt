package dokka.java.api

interface OutputWriter {
    fun write(path: String, text: String, ext: String)

    fun writeResources(pathFrom: String, pathTo: String)
}