package amphip.solver

import scala.sys.process.Process
import scala.util.Try

import better.files._

import amphip.data._
import amphip.sem.mathprog

object GLPSOL extends Solver {

  def solve(model: ModelWithData): Ret = {

    val modelSection = mathprog.genModel(model.model)
    val dataSection = mathprog.genData(model.data)

    val modelPath  = File.newTemporaryFile(prefix = "mathprog-", suffix = "-model")
    val dataPath   = File.newTemporaryFile(prefix = "mathprog-", suffix = "-data")
    val outputPath = File.newTemporaryFile(prefix = "mathprog-", suffix = "-output")

    modelPath.write(modelSection)
    dataPath.write(dataSection)

    val builder = Process("glpsol",
      Seq(
        "--model", modelPath.pathAsString,
        "--data", dataPath.pathAsString,
        "--output", outputPath.pathAsString))

    println(builder)

    (builder.lineStream_!.mkString("\n"), Try(outputPath.contentAsString).getOrElse(""))
  }

}

