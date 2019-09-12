package amphip.data

import scala.annotation.implicitNotFound
import scala.math.BigDecimal.RoundingMode.{ HALF_UP => HalfUp, DOWN => Down }

import java.math.MathContext.{ DECIMAL128 => D128 }

/*
import scalaz.std.list._, listSyntax._
import scalaz.std.option._, optionSyntax._
import scalaz.syntax.foldable1._
import scalaz.syntax.show._
import scalaz.syntax.std.map._
*/
import scalaz.Scalaz._

import spire.math._
import spire.implicits._

import amphip.base._
import amphip.model.ast._
import amphip.model.show._
import amphip.data.SimpleData._
import amphip.data.ModelData._

object eval {

  val key = DataKey

  @implicitNotFound("Eval is not defined for ${A}")
  trait Eval[A, B] {
    def eval(expr: A)(implicit modelData: ModelData): B
  }

  def apply[A, B](expr: A)(implicit modelData: ModelData, Eval: Eval[A, B]): B = Eval.eval(expr)
  def apply[A, B](expr: A, modelData: => ModelData)(implicit Eval: Eval[A, B]): B = Eval.eval(expr)(modelData)

  def from[A, B](f: ModelData => A => B): Eval[A, B] = new Eval[A, B] {
    def eval(a: A)(implicit modelData: ModelData): B = f(modelData)(a)
  }

  // STATEMENTS

  private val PFSetAssing: PartialFunction[SetAtt, List[SetData]] = { 
    case SetAssign(SetLit(values @ _*)) =>  
        values.toList.map {
          case Nil                   => SetTuple(Nil)
          case NumLit   (num) :: Nil => SetVal(SimpleNum(num))
          case StringLit(str) :: Nil => SetVal(SimpleStr(str))
          case l => SetTuple(l.collect {
            case NumLit   (num) => SimpleNum(num)
            case StringLit(str) => SimpleStr(str)
          })
        }
  }
  
  private val PFSetDefault: PartialFunction[SetAtt, List[SetData]] = { 
    case SetDefault(SetLit(values @ _*)) =>  
        values.toList.map {
          case Nil                   => SetTuple(Nil)
          case NumLit   (num) :: Nil => SetVal(SimpleNum(num))
          case StringLit(str) :: Nil => SetVal(SimpleStr(str))
          case l => SetTuple(l.collect {
            case NumLit   (num) => SimpleNum(num)
            case StringLit(str) => SimpleStr(str)
          })
        }
  }

  /**
   * Helper to get the value of an specific attribute.
   * It assumes that the set is expanded, ie, the domain is `none'.
   */
  private def evalAtt(expansion: SetStat, dataPF: PartialFunction[SetAtt, List[SetData]]): Option[List[SetData]] = 
    expansion.atts.collect(dataPF).headOption

  private val PFParamAssing: PartialFunction[ParamAtt, SimpleData] = { 
    case ParamAssign(NumLit   (num)) => SimpleNum(num)
    case ParamAssign(StringLit(str)) => SimpleStr(str) 
  }

  private val PFParamDefault: PartialFunction[ParamAtt, SimpleData] = { 
    case ParamDefault(NumLit   (num)) => SimpleNum(num)
    case ParamDefault(StringLit(str)) => SimpleStr(str)
  }

  /**
   * Helper to get the value of an specific attribute.
   * It assumes that the parameter is expanded, ie, the domain is `none'.
   */
  private def evalAtt(expansion: ParamStat, dataPF: PartialFunction[ParamAtt, SimpleData]): Option[SimpleData] = 
    expansion.atts.collect(dataPF).headOption

  def expand(stat: Stat)(implicit modelData: ModelData): LazyExpansion[Stat] = stat match {
    case x: SetStat        => expand(x)
    case x: ParamStat      => expand(x)
    case x: VarStat        => expand(x)
    case x: ConstraintStat => expand(x)
    case x: ObjectiveStat  => expand(x)
  }

  @inline private[this] def thunk[A](x: => A): () => A = { () => x }

  def expand(set: SetStat)(implicit modelData: ModelData): LazyExpansion[SetStat] = set match {
    case SetStat(name, alias, domain, atts) =>
      domain match {
        case None => LinkedMap(key(name) -> thunk(SetStat(name, alias, none, atts.map(eval(_)))))
        case Some(indExpr) =>
          val pairs =
            for {
              localData <- eval(indExpr)
            } yield {
              val k = key(name, localData.values.toList)
              k -> thunk(SetStat(name, alias, none, atts.map(eval(_, modelData.plusParams(localData)))))
            }

          LinkedMap(pairs: _*)
      }
  }

  def expand(param: ParamStat)(implicit modelData: ModelData): LazyExpansion[ParamStat] = param match {
    case ParamStat(name, alias, domain, atts) =>
      domain match {
        case None => LinkedMap(key(name) -> thunk(ParamStat(name, alias, none, atts.map(eval(_)))))
        case Some(indExpr) =>
          val pairs =
            for {
              localData <- eval(indExpr)
            } yield {
              val k = key(name, localData.values.toList)
              k -> thunk(ParamStat(name, alias, none, atts.map(eval(_, modelData.plusParams(localData)))))
            }

          LinkedMap(pairs: _*)
      }
  }

  def expand(xvar: VarStat)(implicit modelData: ModelData): LazyExpansion[VarStat] = xvar match {
    case VarStat(name, alias, domain, atts) =>
      domain match {
        case None => LinkedMap(key(name) -> thunk(VarStat(name, alias, none, atts.map(eval(_)))))
        case Some(indExpr) =>
          val pairs =
            for {
              localData <- eval(indExpr)
            } yield {
              val k = key(name, localData.values.toList)
              k -> thunk(VarStat(name, alias, none, atts.map(eval(_, modelData.plusParams(localData)))))
            }

          LinkedMap(pairs: _*)
      }
  }

  def expand(ctr: ConstraintStat)(implicit modelData: ModelData): LazyExpansion[ConstraintStat] = ctr match {
    case EqConstraintStat(name, alias, domain, left, right) =>
      domain match {
        case None => LinkedMap(key(name) -> thunk(EqConstraintStat(name, alias, none, eval(left), eval(right))))
        case Some(indExpr) =>
          val pairs =
            for {
              localData <- eval(indExpr)
            } yield {
              val k = key(name, localData.values.toList)
              k -> thunk(EqConstraintStat(name, alias, none, eval(left, modelData.plusParams(localData)), eval(right, modelData.plusParams(localData))))
            }

          LinkedMap(pairs: _*)
      }

    case LTEConstraintStat(name, alias, domain, left, right) =>
      domain match {
        case None => LinkedMap(key(name) -> thunk(LTEConstraintStat(name, alias, none, eval(left), eval(right))))
        case Some(indExpr) =>
          val pairs =
            for {
              localData <- eval(indExpr)
            } yield {
              val k = key(name, localData.values.toList)
              k -> thunk(LTEConstraintStat(name, alias, none, eval(left, modelData.plusParams(localData)), eval(right, modelData.plusParams(localData))))
            }

          LinkedMap(pairs: _*)
      }

    case GTEConstraintStat(name, alias, domain, left, right) =>
      domain match {
        case None => LinkedMap(key(name) -> thunk(GTEConstraintStat(name, alias, none, eval(left), eval(right))))
        case Some(indExpr) =>
          val pairs =
            for {
              localData <- eval(indExpr)
            } yield {
              val k = key(name, localData.values.toList)
              k -> thunk(GTEConstraintStat(name, alias, none, eval(left, modelData.plusParams(localData)), eval(right, modelData.plusParams(localData))))
            }

          LinkedMap(pairs: _*)
      }

    case DLTEConstraintStat(name, alias, domain, lower, expr, upper) =>
      domain match {
        case None => LinkedMap(key(name) -> thunk(DLTEConstraintStat(name, alias, none, NumLit(eval(lower)), eval(expr), NumLit(eval(upper)))))
        case Some(indExpr) =>
          val pairs =
            for {
              localData <- eval(indExpr)
            } yield {
              val k = key(name, localData.values.toList)
              k -> thunk(DLTEConstraintStat(
                name,
                alias,
                none,
                NumLit(eval(lower, modelData.plusParams(localData))),
                eval(expr, modelData.plusParams(localData)),
                NumLit(eval(upper, modelData.plusParams(localData)))))
            }

          LinkedMap(pairs: _*)
      }

    case DGTEConstraintStat(name, alias, domain, lower, expr, upper) =>
      domain match {
        case None => LinkedMap(key(name) -> thunk(DGTEConstraintStat(name, alias, none, NumLit(eval(lower)), eval(expr), NumLit(eval(upper)))))
        case Some(indExpr) =>
          val pairs =
            for {
              localData <- eval(indExpr)
            } yield {
              val k = key(name, localData.values.toList)
              k -> thunk(DGTEConstraintStat(
                name,
                alias,
                none,
                NumLit(eval(lower, modelData.plusParams(localData))),
                eval(expr, modelData.plusParams(localData)),
                NumLit(eval(upper, modelData.plusParams(localData)))))
            }

          LinkedMap(pairs: _*)
      }
  }

  def expand(obj: ObjectiveStat)(implicit modelData: ModelData): LazyExpansion[ObjectiveStat] = obj match {
    case Minimize(name, alias, domain, expr) =>
      domain match {
        case None => LinkedMap(key(name) -> thunk(Minimize(name, alias, none, eval(expr))))
        case Some(indExpr) =>
          val pairs =
            for {
              localData <- eval(indExpr)
            } yield {
              val k = key(name, localData.values.toList)
              k -> thunk(Minimize(name, alias, none, eval(expr, modelData.plusParams(localData))))
            }

          LinkedMap(pairs: _*)
      }

    case Maximize(name, alias, domain, expr) =>
      domain match {
        case None => LinkedMap(key(name) -> thunk(Maximize(name, alias, none, eval(expr))))
        case Some(indExpr) =>
          val pairs =
            for {
              localData <- eval(indExpr)
            } yield {
              val k = key(name, localData.values.toList)
              k -> thunk(Maximize(name, alias, none, eval(expr, modelData.plusParams(localData))))
            }

          LinkedMap(pairs: _*)
      }
  }

  implicit val StatEval: Eval[Stat, Map[DataKey, Stat]] = from(implicit modelData =>
    {
      case x: SetStat => eval(x)

      case x: ParamStat => eval(x)

      case x: VarStat => eval(x)

      case x: ConstraintStat => eval(x)

      case x: ObjectiveStat => eval(x)
    })

  private[this] def force[A](exp: LazyExpansion[A]): Map[DataKey, A] = exp.mapValues(_())

  implicit val SetStatEval: Eval[SetStat, Map[DataKey, SetStat]] = from(implicit modelData => x => force(expand(x)))

  implicit val ParamStatEval: Eval[ParamStat, Map[DataKey, ParamStat]] = from(implicit modelData => x => force(expand(x)))

  implicit val VarStatEval: Eval[VarStat, Map[DataKey, VarStat]] = from(implicit modelData => x => force(expand(x)))

  implicit val ConstraintStatEval: Eval[ConstraintStat, Map[DataKey, ConstraintStat]] = from(implicit modelData => x => force(expand(x)))

  implicit val ObjectiveStatEval: Eval[ObjectiveStat, Map[DataKey, ObjectiveStat]] = from(implicit modelData => x => force(expand(x)))

  implicit val SetAttEval: Eval[SetAtt, SetAtt] = from(implicit modelData =>
    {
      case SetDimen(n) => SetDimen(n)

      case SetWithin(expr) => SetWithin(asSetLit(eval(expr)))

      case SetAssign(expr) => SetAssign(asSetLit(eval(expr)))

      case SetDefault(expr) => SetDefault(asSetLit(eval(expr)))
    })

  implicit val ParamAttEval: Eval[ParamAtt, ParamAtt] = from(implicit modelData =>
    {
      case ParamLT(expr) => ParamLT(eval(expr).fold(NumLit, StringLit))

      case ParamLTE(expr) => ParamLTE(eval(expr).fold(NumLit, StringLit))

      case ParamEq(expr) => ParamEq(eval(expr).fold(NumLit, StringLit))

      case ParamNEq(expr) => ParamNEq(eval(expr).fold(NumLit, StringLit))

      case ParamGT(expr) => ParamGT(eval(expr).fold(NumLit, StringLit))

      case ParamGTE(expr) => ParamGTE(eval(expr).fold(NumLit, StringLit))

      case ParamIn(expr) => ParamIn(asSetLit(eval(expr)))

      case ParamAssign(expr) => ParamAssign(eval(expr).fold(NumLit, StringLit))

      case ParamDefault(expr) => ParamDefault(eval(expr).fold(NumLit, StringLit))

      case x => x
    })

  implicit val VarAttEval: Eval[VarAtt, VarAtt] = from(implicit modelData =>
    {
      case VarLTE(expr) => VarLTE(NumLit(eval(expr)))

      case VarEq(expr) => VarEq(NumLit(eval(expr)))

      case VarGTE(expr) => VarGTE(NumLit(eval(expr)))

      case x => x
    })

  // EXPRESSIONS

  implicit val ExprEval: Eval[Expr, Any] = from(implicit modelData =>
    {
      case x: SimpleExpr => eval(x)

      case x: SetExpr => eval(x)

      case x: LogicExpr => eval(x)

      case x: LinExpr => eval(x)
    })

  // SIMPLE

  implicit val SimpleExprEval: Eval[SimpleExpr, SimpleData] = from(implicit modelData =>
    {
      case x: ParamRef => eval(x)

      case x: DummyIndRef => eval(x)

      case x: NumExpr => SimpleNum(eval(x))

      case x: SymExpr => SimpleStr(eval(x))
    })

  implicit val NumExprWithSymExprEval: Eval[NumExpr with SymExpr, SimpleData] = from(implicit modelData =>
    {
      case x: ParamRef => eval(x)

      case x: DummyIndRef => eval(x)
    })

  implicit val ParamRefEval: Eval[ParamRef, SimpleData] = from(implicit modelData =>
    {
      case ParamRef(param, subscript) =>
        val k = key(param.name, eval(subscript))

        val expParam = 
          modelData.paramsExpansion.get(k) 
          .orElse(expand(param).get(k).map(_())) // only calculates the expansion if it is not preloaded
          .err(s"subscript `${subscript.shows}' does not conform to parameter `${param.name}' definition")

        val assignData  = evalAtt(expParam, PFParamAssing)
        val defaultData = evalAtt(expParam, PFParamDefault)

        // FIXME check lower and upper bounds
        assignData
          .orElse(modelData.params.get(k))
          .orElse(defaultData)
          .err(s"no data found for `$k'")
    })

  implicit val DummyIndRefEval: Eval[DummyIndRef, SimpleData] = from(implicit modelData =>
    {
      case DummyIndRef(dummyInd) =>
        modelData.params.get(key(dummyInd.name))
          .err(s"dummy index `${dummyInd.name}' out of scope")
    })

  // NUMERIC
  implicit val NumExprEval: Eval[NumExpr, BigDecimal] = from(implicit modelData =>
    {
      case CondNumExpr(test, ifTrue, otherwise) => if (eval(test)) eval(ifTrue) else otherwise.fold[BigDecimal](0)(eval(_))

      case NumAdd(left, right) => eval(left) + eval(right)

      case NumSub(left, right) => eval(left) - eval(right)

      case NumLess(left, right) => max(eval(left) - eval(right), 0)

      case NumSum(indexing, integrand) => eval(indexing -> integrand).sum

      case NumProd(indexing, integrand) => eval(indexing -> integrand).product

      case NumMax(indexing, integrand) => eval(indexing -> integrand).max

      case NumMin(indexing, integrand) => eval(indexing -> integrand).min

      case NumMult(left, right) => eval(left) * eval(right)

      case NumDiv(left, right) => eval(left).apply(D128) / eval(right)

      case NumDivExact(left, right) => eval(left) /~ eval(right)

      case NumMod(left, right) => eval(left) % eval(right)

      case NumUnaryPlus(expr) => eval(expr)

      case NumUnaryMinus(expr) => -eval(expr)

      case NumRaise(left, right) => eval(left) fpow eval(right)

      case x: ParamRef =>
        eval(x).fold(identity, _ => sys.error(typeMismatchNumExpr(x.param.name, "param")))

      case x: DummyIndRef =>
        eval(x).fold(identity, _ => sys.error(typeMismatchNumExpr(x.dummyInd.name, "dummy index")))

      case x: NumFuncRef => eval(x)

      case NumLit(num) => num
    })

  // XXX 😬 🤔 
  private val rnd = new scala.util.Random()

  implicit val NumFuncRefEval: Eval[NumFuncRef, BigDecimal] = from(implicit modelData =>
    {
      case Abs(expr) => eval(expr).abs

      case Atan(expr) => atan(eval(expr))

      case Atan2(x1, x2) => atan2(eval(x1), eval(x2))

      case Card(expr) => eval(expr).size

      case Ceil(expr) => ceil(eval(expr))

      case Cos(expr) => cos(eval(expr))

      case Exp(expr) => exp(eval(expr))

      case Floor(expr) => floor(eval(expr))

      case Gmtime() => new java.util.Date().getTime

      case Length(expr) => eval(expr).length

      case Log(expr) => log(eval(expr))

      case Log10(expr) => log10(eval(expr).toDouble) // no implementation of log10 for BigDecimal ...

      case Max(expr @ _*) => expr.map(eval(_)).max

      case Min(expr @ _*) => expr.map(eval(_)).min

      case Round(expr, n) => n.fold(round(eval(expr)))(n => eval(expr).setScale(eval(n).toIntExact, HalfUp))

      case Sin(expr) => sin(eval(expr))

      case Sqrt(expr) => sqrt(eval(expr))

      case Str2time(s @_, f @_) => sys.error("`str2time' not yet supported")

      case Trunc(expr, n) => {
        def trunc(x: BigDecimal, n: BigDecimal): BigDecimal = x.setScale(n.toIntExact, Down)

        n.fold(trunc(eval(expr), 0))(n => trunc(eval(expr), eval(n)))
      }

      case Irand224() => rnd.nextInt(2 ** 24)

      case Uniform01() => rnd.nextDouble()
    })

  // SYMBOLIC
  implicit val SymExprEval: Eval[SymExpr, String] = from(implicit modelData =>
    {
      case CondSymExpr(test, ifTrue, otherwise) => if (eval(test)) eval(ifTrue) else otherwise.fold("")(eval(_))

      case Concat(left, right) => eval(left) + eval(right)

      case SymNumExpr(expr) => eval(expr).toString

      case x: ParamRef => eval(x).fold(_.toString, identity)

      case x: DummyIndRef => eval(x).fold(_.toString, identity)

      case x: SymFuncRef => eval(x)

      case StringLit(str) => str
    })

  implicit val SymFuncRefEval: Eval[SymFuncRef, String] = from(implicit modelData =>
    {
      case Substr(expr, from, length) =>
        val text = eval(expr)
        val start = eval(from).toIntExact
        val end = start + length.fold(text.length)(l => eval(l).toIntExact)

        text.substring(start, end)

      case Time2str(t @_, f @_) => sys.error("`time2str' not yet supported")
    })

  // SET
  implicit val SetExprEval: Eval[SetExpr, List[SetData]] = from(implicit modelData =>
    {
      case CondSetExpr(test, ifTrue, otherwise) => if (eval(test)) eval(ifTrue) else eval(otherwise)

      case Union(left, right) => (eval(left) ::: eval(right)).distinct

      case Diff(left, right) => eval(left).diff(eval(right))

      case SymDiff(left, right) =>
        val l = eval(left)
        val r = eval(right)
        l.union(r).diff(l.intersect(r)).distinct

      case Inter(left, right) => eval(left).intersect(eval(right))

      case Cross(left, right) =>
        for {
          x <- eval(left)
          y <- eval(right)
        } yield {
          (x, y) match {
            case (SetVal(x), SetVal(y)) => SetTuple(List(x, y))
            case (SetVal(x), SetTuple(y)) => SetTuple(x :: y)
            case (SetTuple(x), SetVal(y)) => SetTuple(x ::: List(y))
            case (SetTuple(x), SetTuple(y)) => SetTuple(x ::: y)
          }

        }

      case SetOf(indexing, integrand) => eval(indexing -> integrand)

      case ArithSet(t0, tf, deltaT) =>
        val t0Val = eval(t0)
        val tfVal = eval(tf)
        val deltaTVal = deltaT.fold[BigDecimal](1)(eval(_))

        (t0Val to tfVal by deltaTVal).map(x => SetVal(SimpleNum(x))).toList

      case x: SetRef => eval(x)

      case SetLit(values @ _*) =>
        values.toList.map {
          case Nil => SetTuple(Nil)
          case x :: Nil => SetVal(eval(x))
          case l => SetTuple(eval(l))
        }

      case IndExprSet(indexing) =>
        eval(indexing).map(_.values.toList).map {
          case Nil => SetTuple(Nil)
          case x :: Nil => SetVal(x)
          case l => SetTuple(l)
        }
    })

  implicit val SetRefEval: Eval[SetRef, List[SetData]] = from(implicit modelData => {
    case SetRef(set, subscript) =>
      val k = key(set.name, eval(subscript))

      val expSet =
        modelData.setsExpansion.get(k) 
        .orElse(expand(set).get(k).map(_())) // only calculates the expansion if it is not preloaded
        .err(s"subscript `${subscript.shows}' does not conform to set `${set.name}' definition")

      val assignData  = evalAtt(expSet, PFSetAssing)
      val defaultData = evalAtt(expSet, PFSetDefault)

      assignData
        .orElse(modelData.sets.get(k))
        .orElse(defaultData)
        .err(s"no data found for `$k'")
  })

  // INDEXING

  implicit val IndExprEval: Eval[IndExpr, IndexingData] = from(implicit modelData =>
    {
      expr =>
        val entriesData = expr.entries.foldLeft(List(LinkedMap.empty[DataKey, SimpleData])) { (lastData, entry) =>
          for {
            data <- lastData
            newData <- eval(entry, modelData.plusParams(data))
          } yield {
            data ++ newData
          }
        }

        entriesData.filter { data =>
          expr.predicate.fold(true)(f => eval(f, modelData.plusParams(data)))
        }
    })

  implicit val IndEntryEval: Eval[IndEntry, IndexingData] = from(implicit modelData =>
    {
      case IndEntry(indices, set, predicate) =>

        def localData(indices: List[DummyIndDecl], setD: SetData): LinkedMap[DataKey, SimpleData] =
          (indices, setD) match {
            case (Nil, SetTuple(Nil)) => LinkedMap.empty
            case (i :: Nil, SetVal(x)) => LinkedMap(key(i.name) -> x)
            case (i :: is, SetTuple(x :: xs)) => LinkedMap(key(i.name) -> x) ++ localData(is, SetTuple(xs))
            case (_, SetVal(_)) => sys.error(s"`${indices.shows}' has incompatible size for `${set.shows}'")
            case (_, SetTuple(_)) => sys.error(s"`${indices.shows}' has incompatible size for `${set.shows}'")
          }

        val setEv = eval(set)
        val effInd = effectiveIndices(setEv, indices)

        val filtered = setEv.map(localData(effInd, _))
          .filter { lData =>
            predicate.fold(true)(f => eval(f, modelData.plusParams(lData)))
          }

        val predExprMap = predicate.fold(Map.empty[String, SimpleExpr])(p => IndEntry.extract(p).mapKeys(_.name))

       /*
        * the final expression must have only the values not appearing in the predicate expression
        */
        filtered.map(_.filter { case (k, _) => predExprMap.get(k.name).isEmpty })
    })

  def effectiveIndices(setEv: List[SetData], indices: List[DummyIndDecl], nameHint: Option[String] = None, gen: Gen = gen): List[DummyIndDecl] = {
    if (indices.isEmpty) {
      val dimen = setEv.headOption.fold(0)(_ match {
        case SetVal(_) => 1
        case SetTuple(xs) => xs.size
      })
      List.fill(dimen)(DummyIndDecl(gen.dummy(nameHint).freshName, synthetic = true))
    } else {
      indices
    }
  }

  implicit val IndExprNumIntegrandEval: Eval[(IndExpr, NumExpr), List[BigDecimal]] = from(implicit modelData =>
    {
      case (indexing, integrand) =>
        for {
          localData <- eval(indexing)
        } yield {
          eval(integrand, modelData.plusParams(localData))
        }
    })

  implicit val IndExprSetIntegrandEval: Eval[(IndExpr, List[SimpleExpr]), List[SetData]] = from(implicit modelData =>
    {
      case (indexing, integrand) =>
        for {
          localData <- eval(indexing)
        } yield {
          integrand match {
            case Nil => SetTuple(Nil)
            case x :: Nil => SetVal(eval(x, modelData.plusParams(localData)))
            case l => SetTuple(eval(l, modelData.plusParams(localData)))
          }
        }
    })

  implicit val IndExprLogicIntegrandEval: Eval[(IndExpr, LogicExpr), List[Boolean]] = from(implicit modelData =>
    {
      case (indexing, integrand) =>
        for {
          localData <- eval(indexing)
        } yield {
          eval(integrand, modelData.plusParams(localData))
        }
    })

  // LOGIC
  implicit val LogicExprEval: Eval[LogicExpr, Boolean] = from(implicit modelData =>
    {
      case Disj(left, right) => eval(left) || eval(right)

      case Forall(indexing, integrand) => eval(indexing -> integrand).forall(identity)

      case Exists(indexing, integrand) => eval(indexing -> integrand).exists(identity)

      case Conj(left, right) => eval(left) && eval(right)

      case Neg(expr) => !eval(expr)

      case LT(left, right) => eval(left) < eval(right)

      case LTE(left, right) => eval(left) <= eval(right)

      case GT(left, right) => eval(left) > eval(right)

      case GTE(left, right) => eval(left) >= eval(right)

      case NEq(left, right) => eval(left) =!= eval(right)

      case Eq(left, right) => eval(left) === eval(right)

      case NotIn(values, set) =>
        val ev = eval(values)
        !eval(set).exists(x => (ev, x) match {
          case (Nil, SetTuple(Nil)) => true
          case (i :: Nil, SetVal(x)) => i == x
          case (is, SetTuple(xs)) => is == xs
          case _ => false
        })

      case In(values, set) =>
        val ev = eval(values)
        eval(set).exists(x => (ev, x) match {
          case (Nil, SetTuple(Nil)) => true
          case (i :: Nil, SetVal(x)) => i == x
          case (is, SetTuple(xs)) => is == xs
          case _ => false
        })

      case NotWithin(left, right) =>
        val r = eval(right).toSet
        !eval(left).forall(r)

      case Within(left, right) =>
        val r = eval(right).toSet
        eval(left).forall(r)

      case x: NumExpr => eval(x) != 0
    })

  //LINEAR
  implicit val LinExprEval: Eval[LinExpr, LinExpr] = from(implicit modelData =>
    {
      case CondLinExpr(test, ifTrue, otherwise) => if (eval(test)) eval(ifTrue) else otherwise.fold[LinExpr](NumLit(0))(eval(_))

      case LinAdd(left, right) => LinAdd(eval(left), eval(right))

      case LinSub(left, right) => LinSub(eval(left), eval(right))

      case LinSum(indexing, integrand) =>
        val evIntegrand =
          for {
            localData <- eval(indexing)
          } yield {
            eval(integrand, modelData.plusParams(localData))
          }

        LinSumExp(evIntegrand)

      case LinSumExp(summands) => LinSumExp(summands.map(eval(_)))

      /* case LinSum(indexing, integrand) =>
        val evIntegrand =
          for {
            localData <- eval(indexing)
          } yield {
            eval(integrand, modelData.plusParams(localData))
          }

        evIntegrand.toNel.fold[LinExpr](NumLit(0))(_.foldLeft1((expr1, expr2) => LinAdd(expr1, expr2))) */

      case LinMult(left, right) => LinMult(NumLit(eval(left)), eval(right))

      case LinDiv(left, right) => LinDiv(eval(left), NumLit(eval(right)))

      case LinUnaryPlus(x) => LinUnaryPlus(eval(x))

      case LinUnaryMinus(x) => LinUnaryMinus(eval(x))

      case x: VarRef => eval(x)

      case x: NumExpr => NumLit(eval(x))
    })

  implicit val VarRefEval: Eval[VarRef, VarRef] = from(implicit modelData => {
    case VarRef(xvar, subscript) => VarRef(xvar, eval(subscript).map(_.fold(NumLit(_), StringLit(_))))
  })

  // BASIC

  implicit val SubscriptEval: Eval[List[SimpleExpr], List[SimpleData]] = from(implicit modelData => _.map(eval(_)))

  def typeMismatchNumExpr(decl: SymName, declType: String) = s"$declType `$decl' has incorrect type. Expected `NumExpr', found `SymExpr'."

  private def asSetLit(data: List[SetData]): SetLit = {
    val tuples =
      data.map {
        case SetVal(x) => List(x.fold(NumLit, StringLit))
        case SetTuple(values) => values.map(_.fold(NumLit, StringLit))
      }
    SetLit(tuples: _*)
  }
}

