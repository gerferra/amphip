package amphip.sem.mathprog

import scalaz._, Scalaz._

import amphip.base.implicits._
import amphip.data._
import amphip.data.ModelData._
import amphip.data.show._


object genData {

  def apply(modelData: ModelData): String = modelData match {
    case ModelData(params, sets, _, _, _) =>
      getSetData(sets) + "\n" + getParamData(params) + "\nend;\n"
  }

  /**
   *  Basic support for transforming the EDSL parameter data format to MathpProg data format.
   */
  def getParamData(params: ParamStatData): String = {
    val str =
      for {
        (name, valuesNE) <- params.toSeq.groupByLinked { case (key, _) => key.name }
      } yield {
        val (key0, value0) = valuesNE.head
        key0.subscript match {

          case Nil =>
            s"param $name := ${value0.shows};\n"

          case _ :: Nil =>
            s"param $name := ${valuesNE.map { case (k, v) => s"${k.subscript(0).shows} ${v.shows}" }.mkString(", ")};\n"

          case _ :: _ :: Nil =>
            if (isTabular(valuesNE)) {
              s"""param $name:
                 |${tableString(name, valuesNE)};\n""".stripMargin
            } else {
              simpleValuesString(name, valuesNE)
            }

          case _ =>
            val valuesByQual = valuesNE.groupByLinked { case (k, _) => k.subscript.drop(2) }
            val allTabular = valuesByQual.forall(x => isTabular(x._2))
            if (allTabular) {
              val tablesStrings =
                for {
                  (qual, valuesNE) <- valuesByQual
                } yield {
                  s"""[*, *, ${qual.map(_.shows).mkString(", ")}]:
                  |${tableString(name, valuesNE)}""".stripMargin
                }

              s"""param $name :=
                 |${tablesStrings.mkString("", "\n\n", ";\n")}""".stripMargin
            } else {
              simpleValuesString(name, valuesNE)
            }
        }
      }

    str.mkString(f"%n")
  }

  def isTabular(valuesNE: Seq[(DataKey, SimpleData)]): Boolean = {
    val valuesByRow = valuesNE.groupByLinked { case (k, _) => k.subscript(0) }
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

    rowColumns.toSeq.distinct.size == 1
  }

  private def simpleValuesString(pName: String, valuesNE: Seq[(DataKey, SimpleData)]): String = {
    val valuesByQual = valuesNE.groupByLinked { case (k, _) => k.subscript.dropRight(1) }
    val tablesStrings =
      for {
        (qual, valuesNE) <- valuesByQual
      } yield {
        val values = valuesNE.map { case (k, v) => s"${k.subscript.last.shows} ${v.shows}" }.mkString(", ")
        s"""[${qual.map(_.shows).mkString(", ")}, *] := $values"""
      }

    s"""param $pName :=
       |${tablesStrings.mkString("", "\n", ";\n")}""".stripMargin
  }

  private def tableString(pName: String, valuesNE: Seq[(DataKey, SimpleData)]): String = {
    val (keys, values) = valuesNE.unzip
    val rows = keys.map(_.subscript(0)).distinct
    val cols = keys.map(_.subscript(1)).distinct
    val valuesByRow = valuesNE.groupByLinked { case (k, _) => k.subscript(0) }
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
    if (!tabular) println(s"warn: data is not tabular for parameter `$pName'")

    val valuesByRowCol =
      for {
        (k, values) <- valuesByRow
      } yield {
        k -> values.groupByLinked {
          case (k, _) => k.subscript(1)
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
        val tryValue = 
          scala.util.Try(valuesByRowCol(r)(c).head._2) // if the subscript is repeated, it will take only the first
          .recover {
            case _: NoSuchElementException =>
              val info = if (!isTabular(valuesNE)) ". Data is not tabular" else ""
              throw new NoSuchElementException(s"no value for $pName[${r.shows},${c.shows}...]$info")
          }
        val value = tryValue.get 
        res.append(s"%${C}s ".format(value.shows))
      }
      res.append(if (rowsSize == (i + 1)) f"" else f"%n")
    }

    res.toString

  }

  def getSetData(sets: SetStatData): String = {
    val str =
      for {
        (name, valuesNE) <- sets.toSeq.groupByLinked { case (key, _) => key.name }
      } yield {
        val indStr =
          for {
            (k, v) <- valuesNE
          } yield {
            s"set ${k.shows} := ${v.shows};"
          }

        indStr.mkString("", f"%n", f"%n")

      }

    str.mkString(f"%n")
  }
}