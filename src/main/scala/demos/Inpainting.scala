package demos

import java.nio.file.Paths

import cogdebugger.CogDebuggerApp
import cogio.ColorMovie
import libcog._
import toolkit.computervision.colorfunctions.Luma
import toolkit.solvers

object Inpainting extends CogDebuggerApp (
  new ComputeGraph {
    val path = Paths.get("src", "main", "resources", "action-60fps.mp4").toString
    val movie = ColorMovie(path, synchronous = true)(14 to 525, 224 to 735).toVectorField

    val sourceMask = ScalarField(512, 512, (x, y) => math.sin((x + y).toFloat / 10f).toFloat) < 0.60f
    val maskedMovie = movie * sourceMask

    val lastFrame = ScalarField(movie.fieldShape)
    lastFrame <== Luma(maskedMovie)

    val motion = abs(Luma(maskedMovie) - lastFrame)

    val shuntingNet = ScalarField.random(sourceMask.fieldShape)
    shuntingNet <== shuntingNet + (-0.005f * shuntingNet + (1f - shuntingNet) * motion)

    val learnedMask = shuntingNet > 0.2f

    val diffused = solvers.DiffuseDirichlet(maskedMovie * learnedMask, learnedMask)

    probe(movie.toColorField, "movie")
    probe(maskedMovie.toColorField, "masked movie")
    probe(diffused.toColorField, "diffused")
    probe(motion)
    probe(shuntingNet)
  }
)
