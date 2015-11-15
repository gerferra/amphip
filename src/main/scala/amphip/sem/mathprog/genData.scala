package amphip.sem.mathprog

import scalaz._, Scalaz._

import amphip.base.implicits._
import amphip.data._
import amphip.data.ModelData._
import amphip.data.show._


object genData {

  def apply(modelData: ModelData): String = modelData match {
    case ModelData(params, sets) =>
      getSetData(sets) + "\n\n" + getParamData(params) + "\nend;\n"
  }

  /**
   *  Very basic support for transforming the EDSL parameter data format to MathpProg data format.
   *  Doesn't handle non-tabular data.
   */
  def getParamData(params: ParamStatData): String = {
    val str =
      for {
        (name, valuesNE) <- params.toSeq.groupByLinked { case (key, value) => key.name }
      } yield {
        val (key0, value0) = valuesNE.head
        //println(name -> key0 -> value0)
        // TODO handle non tabular data
        key0.subscript match {

          case Nil =>
            s"param $name := ${value0.shows};"

          case _ :: Nil =>
            s"param $name := ${valuesNE.map { case (k, v) => s"${k.subscript(0).shows} ${v.shows}" }.mkString(", ")};"

          case _ :: _ :: Nil =>
            s"""param $name:
               |${tableString(valuesNE)};\n""".stripMargin

          case _ =>

            val valuesByQual = valuesNE.groupByLinked { case (k, v) => k.subscript.drop(2) }
            val tablesStrings =
              for {
                (qual, valuesNE) <- valuesByQual
              } yield {
                s"""[*, *, ${qual.map(_.shows).mkString(", ")}]:
                |${tableString(valuesNE)}""".stripMargin
              }

            s"""param $name :=
               |${tablesStrings.mkString("", "\n\n", ";\n")}""".stripMargin

        }
      }

    str.mkString(f"%n")
  }

  private def tableString(valuesNE: Seq[(DataKey, SimpleData)]): String = {
    val (keys, values) = valuesNE.unzip
    val rows = keys.map(_.subscript(0)).distinct
    val cols = keys.map(_.subscript(1)).distinct
    val valuesByRow = valuesNE.groupByLinked { case (k, v) => k.subscript(0) }
    val rowColumns =
      for {
        seq <- valuesByRow.values
      } yield {
        for {
          (k, v) <- seq
        } yield {
          k.subscript(1)
        }
      }

    val tabular = rowColumns.toSeq.distinct.size == 1

    //println(s"tabular? $tabular")

    val valuesByRowCol =
      for {
        (k, values) <- valuesByRow
      } yield {
        k -> values.groupByLinked {
          case (k, v) => k.subscript(1)
        }
      }
    val rowsSize = rows.size
    val R = rows.map(_.shows.length).max
    val C0 = cols.map(_.shows.length).max
    val V = values.map(_.shows.length).max
    val C = math.max(C0, V)

    val res = new StringBuilder()
    // columns row
    res.append(" " * (R + 3))
    for {
      c <- cols
    } {
      res.append(s"%${C}s ".format(c.shows))
    }
    res.append(f":= %n")
    // other rows
    for {
      (r, i) <- rows.zipWithIndex
    } {
      res.append("  ")
      res.append(s"%${R}s ".format(r.shows))
      for {
        c <- cols
      } {
        val value = valuesByRowCol(r)(c).unzip._2.head // if the subscript is repeated, it will take only the first
        res.append(s"%${C}s ".format(value.shows))
      }
      res.append(if (rowsSize == (i + 1)) f"" else f"%n")
    }

    res.toString

  }

  def getSetData(sets: SetStatData): String = {
    val str =
      for {
        (name, valuesNE) <- sets.toSeq.groupByLinked { case (key, value) => key.name }
      } yield {
        val indStr =
          for {
            (k, v) <- valuesNE
          } yield {
            s"set ${k.shows} := ${v.shows};"
          }

        indStr.mkString(f"%n")

      }

    str.mkString(f"%n")
  }
}