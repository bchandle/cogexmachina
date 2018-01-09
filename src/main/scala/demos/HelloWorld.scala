package demos

import java.nio.file.Paths

import cogdebugger.CogDebuggerApp
import cogio.ColorMovie
import libcog._

object HelloWorld extends CogDebuggerApp(
  new ComputeGraph {
    val path = Paths.get("src", "main", "resources", "courtyard.mp4").toString
    val movie = ColorMovie(path, synchronous = false).toVectorField
    val background = VectorField(movie.fieldShape, Shape(3))

    background <== 0.999f * background + 0.001f * movie
    val suspicious = reduceSum(abs(background - movie))

    probe(movie.toColorField, "movie")
    probe(background.toColorField, "background")
    probe(suspicious)
  }
)

