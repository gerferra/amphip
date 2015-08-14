package amphip.solver

import scala.sys.process.Process
import scala.util.Try

import scalax.file.Path

import amphip.data._
import amphip.sem.mathprog

object GLPSOL extends Solver {

  def solve(model: ModelWithData): Ret = {

    val modelSection = mathprog.genModel(model.model)
    val dataSection = mathprog.genData(model.data)

    val modelPath = Path.createTempFile(prefix = "mathprog-", suffix = "-model")
    val dataPath = Path.createTempFile(prefix = "mathprog-", suffix = "-data")
    val outputPath = Path.createTempFile(prefix = "mathprog-", suffix = "-output")

    modelPath.write(modelSection)
    dataPath.write(dataSection)

    val builder = Process("glpsol",
      Seq(
        "--model", modelPath.path,
        "--data", dataPath.path,
        "--output", outputPath.path))

    println(builder)

    (builder.lineStream_!.mkString("\n"), Try(outputPath.string).getOrElse(""))
  }

}

