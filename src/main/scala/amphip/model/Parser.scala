package amphip.model

import scala.util.parsing.combinator._
import scala.util.parsing.input.Position
import scala.util.parsing.input.CharArrayReader.EofCh

import scalaz.{ \/, -\/, \/-, StateT, Scalaz }, Scalaz._

import amphip.base._
import amphip.model.ast._
import amphip.model.show._

import Parser._

/**
 * Parser for MathProg model files.
 * Based on http://www.cs.unb.ca/~bremner/docs/glpk/gmpl.pdf
 */
class Parser extends RegexParsers with PackratParsers {

  type P[A] = Parser[S[A]]

  type PP[A] = PackratParser[S[A]]

  val gen = ast.newGen

  // MODEL

  lazy val pModel: PP[Model] = rep(pStat) <~ opt("end;") ^^ (statsS => for { stats <- statsS.sequence } yield Model(stats))

  // STATEMENTS 

  lazy val pStat: PP[Stat] =
    pSetStat.as[Stat] |
      pParamStat.as[Stat] |
      pVarStat.as[Stat] |
      pConstraintStat.as[Stat] |
      pObjectiveStat.as[Stat]

  lazy val pSetStat: PP[SetStat] =
    "set" ~> wpos(pSymName) ~ opt(pStringLit) ~ opt(pIndExpr) ~ (opt(",") ~> repsep(pSetAtt, opt(","))) <~ ";" ^^ {
      case (name, pos) ~ alias ~ domainS ~ attsS =>
        for {
          p <- scoped(domainS.sequence, attsS.sequence)
          domain = p._1
          atts = p._2
          set <- putDecl(SetStat(name, alias, domain, atts), pos)
        } yield {
          set
        }
    }

  lazy val pParamStat: PP[ParamStat] =
    "param" ~> wpos(pSymName) ~ opt(pStringLit) ~ opt(pIndExpr) ~ (opt(",") ~> repsep(pParamAtt, opt(","))) <~ ";" ^^ {
      case (name, pos) ~ alias ~ domainS ~ attsS =>
        for {
          p <- scoped(domainS.sequence, attsS.sequence)
          domain = p._1
          atts = p._2
          param <- putDecl(ParamStat(name, alias, domain, atts), pos)
        } yield {
          param
        }
    }

  lazy val pVarStat: PP[VarStat] =
    "var" ~> wpos(pSymName) ~ opt(pStringLit) ~ opt(pIndExpr) ~ (opt(",") ~> repsep(pVarAtt, opt(","))) <~ ";" ^^ {
      case (name, pos) ~ alias ~ domainS ~ attsS =>
        for {
          p <- scoped(domainS.sequence, attsS.sequence)
          domain = p._1
          atts = p._2
          xvar <- putDecl(VarStat(name, alias, domain, atts), pos)
        } yield {
          xvar
        }
    }

  lazy val pConstraintStat: PP[ConstraintStat] = {
    val base = opt("subject" ~ "to" | "subj" ~ "to" | "s.t.") ~> pSymName ~ opt(pStringLit) ~ opt(pIndExpr) ~ ":"

    base ~ pNumExpr ~ opt(",") ~ ">=" ~ pLinExpr ~ opt(",") ~ ">=" ~ pNumExpr <~ ";" ^^ {
      case name ~ alias ~ domainS ~ _ ~ uS ~ _ ~ _ ~ fS ~ _ ~ _ ~ lS =>
        for {
          _ <- putScope
          domain <- domainS.sequence
          u <- uS
          f <- fS
          l <- lS
          _ <- removeScope
        } yield {
          DGTEConstraintStat(name, alias, domain, u, f, l): ConstraintStat
        }
    } |
      base ~ pNumExpr ~ opt(",") ~ "<=" ~ pLinExpr ~ opt(",") ~ "<=" ~ pNumExpr <~ ";" ^^ {
        case name ~ alias ~ domainS ~ _ ~ lS ~ _ ~ _ ~ fS ~ _ ~ _ ~ uS =>
          for {
            _ <- putScope
            domain <- domainS.sequence
            l <- lS
            f <- fS
            u <- uS
            _ <- removeScope
          } yield {
            DLTEConstraintStat(name, alias, domain, l, f, u): ConstraintStat
          }
      } |
      base ~ pLinExpr ~ opt(",") ~ ">=" ~ pLinExpr <~ ";" ^^ {
        case name ~ alias ~ domainS ~ _ ~ fS ~ _ ~ _ ~ gS =>
          for {
            _ <- putScope
            domain <- domainS.sequence
            f <- fS
            g <- gS
            _ <- removeScope
          } yield {
            GTEConstraintStat(name, alias, domain, f, g): ConstraintStat
          }
      } |
      base ~ pLinExpr ~ opt(",") ~ "<=" ~ pLinExpr <~ ";" ^^ {
        case name ~ alias ~ domainS ~ _ ~ fS ~ _ ~ _ ~ gS =>
          for {
            _ <- putScope
            domain <- domainS.sequence
            f <- fS
            g <- gS
            _ <- removeScope
          } yield {
            LTEConstraintStat(name, alias, domain, f, g): ConstraintStat
          }
      } |
      base ~ pLinExpr ~ opt(",") ~ "=" ~ pLinExpr <~ ";" ^^ {
        case name ~ alias ~ domainS ~ _ ~ fS ~ _ ~ _ ~ gS =>
          for {
            _ <- putScope
            domain <- domainS.sequence
            f <- fS
            g <- gS
            _ <- removeScope
          } yield {
            EqConstraintStat(name, alias, domain, f, g): ConstraintStat
          }
      }

  }

  lazy val pObjectiveStat: PP[ObjectiveStat] =
    "maximize" ~> pSymName ~ opt(pStringLit) ~ opt(pIndExpr) ~ ":" ~ pLinExpr <~ ";" ^^ {
      case name ~ alias ~ domainS ~ _ ~ expressionS =>
        for {
          p <- scoped(domainS.sequence, expressionS)
          domain = p._1
          expression = p._2
        } yield {
          Maximize(name, alias, domain, expression): ObjectiveStat
        }
    } |
      "minimize" ~> pSymName ~ opt(pStringLit) ~ opt(pIndExpr) ~ ":" ~ pLinExpr <~ ";" ^^ {
        case name ~ alias ~ domainS ~ _ ~ expressionS =>
          for {
            p <- scoped(domainS.sequence, expressionS)
            domain = p._1
            expression = p._2
          } yield {
            Minimize(name, alias, domain, expression): ObjectiveStat
          }
      }

  lazy val pSetAtt: PP[SetAtt] =
    "dimen" ~> pDimen ^^ (x => ev.state(SetDimen(x): SetAtt)) |
      "within" ~> pSetExpr ^^ (_.map(SetWithin(_): SetAtt)) |
      ":=" ~> pSetExpr ^^ (_.map(SetAssign(_): SetAtt)) |
      "default" ~> pSetExpr ^^ (_.map(SetDefault(_): SetAtt))

  lazy val pParamAtt: PP[ParamAtt] =
    "integer" ^^^ (ev.state(Integer: ParamAtt)) |
      "binary" ^^^ (ev.state(Binary: ParamAtt)) |
      "symbolic" ^^^ (ev.state(Symbolic: ParamAtt)) |
      "<" ~> pSimpleExpr ^^ (_.map(ParamLT(_): ParamAtt)) |
      "<=" ~> pSimpleExpr ^^ (_.map(ParamLTE(_): ParamAtt)) |
      ">" ~> pSimpleExpr ^^ (_.map(ParamGT(_): ParamAtt)) |
      ">=" ~> pSimpleExpr ^^ (_.map(ParamGTE(_): ParamAtt)) |
      ("==" | "=") ~> pSimpleExpr ^^ (_.map(ParamEq(_): ParamAtt)) |
      ("<>" | "!=") ~> pSimpleExpr ^^ (_.map(ParamNEq(_): ParamAtt)) |
      "in" ~> pSetExpr ^^ (_.map(ParamIn(_): ParamAtt)) |
      ":=" ~> pSimpleExpr ^^ (_.map(ParamAssign(_): ParamAtt)) |
      "default" ~> pSimpleExpr ^^ (_.map(ParamDefault(_): ParamAtt))

  lazy val pVarAtt: PP[VarAtt] =
    "integer" ^^^ (ev.state(Integer: VarAtt)) |
      "binary" ^^^ (ev.state(Binary: VarAtt)) |
      "<=" ~> pNumExpr ^^ (_.map(VarLTE(_): VarAtt)) |
      ">=" ~> pNumExpr ^^ (_.map(VarGTE(_): VarAtt)) |
      ("==" | "=") ~> pNumExpr ^^ (_.map(VarEq(_): VarAtt))

  ////
  // EXPRESSIONS 

  lazy val pExpr: PP[Expr] =
    pNumExpr.as[Expr] |
      pSymExpr.as[Expr] |
      pSetExpr.as[Expr] |
      pLogicExpr.as[Expr] |
      pLinExpr.as[Expr]

  // SIMPLE

  lazy val pSimpleExpr: PP[SimpleExpr] = pNumExpr.as[SimpleExpr] | pSymExpr.as[SimpleExpr]

  lazy val pSimpleExprOrDummyIndDecl: PP[SimpleExpr \/ DummyIndDecl] = Parser { in =>
    val resSimpleExpr = pSimpleExpr(in)
    val resSymName = pRefSymName(in)

    val resSimpleExprAsDisj = resSimpleExpr.map(_.map(_.left[DummyIndDecl]))

    resSymName match {
      case Success(name, nName) => resSimpleExprAsDisj match {
        case Success(sexprS, nSExpr) if nName.offset == nSExpr.offset =>
          /*
           * We have a SimpleExpr that can be a DummyInd          
           * If it is a missed reference, it is treated as a DummyIndDecl
           */
          val newSExprS =
            for {
              symTab <- ev.get
              x <- if (symTab.contains(name)) sexprS else putDecl(DummyIndDecl(name), nSExpr.pos).map(_.right)
            } yield {
              x: SimpleExpr \/ DummyIndDecl
            }

          Success(newSExprS, nSExpr)

        case _ => resSimpleExprAsDisj
      }
      case _ => resSimpleExprAsDisj
    }

  }

  lazy val pSimpleRef: PP[NumExpr with SymExpr] = wpos(pRefSymName) ^^ {
    case (name, pos) =>
      for {
        disj <- refDisj[ParamStat, DummyIndDecl](name, pos)
      } yield {

        disj match {
          case -\/(a) => ParamRef(a): NumExpr with SymExpr
          case \/-(b) => DummyIndRef(b): NumExpr with SymExpr
        }

      }
  }

  lazy val pParamRef: PP[ParamRef] = pSubscriptedParamRef | pUnsubscriptedParamRef

  lazy val pUnsubscriptedParamRef: PP[ParamRef] = wpos(pRefSymName) ^^ {
    case (name, pos) =>
      for {
        param <- ref[ParamStat](name, pos)
      } yield {
        ParamRef(param)
      }
  }

  lazy val pSubscriptedParamRef: PP[ParamRef] = wpos(pRefSymName) ~ pSubscript ^^ {
    case (name, pos) ~ subscriptS =>
      for {
        param <- ref[ParamStat](name, pos)
        subscript <- subscriptS
      } yield {
        ParamRef(param, subscript)
      }
  }

  def pDummyIndRef: P[DummyIndRef] = wpos(pRefSymName) ^^ {
    case (name, pos) =>
      for {
        dummyInd <- ref[DummyIndDecl](name, pos)
      } yield {
        DummyIndRef(dummyInd)
      }
  }

  lazy val pSubscript: PP[List[SimpleExpr]] = "[" ~> rep1sep(pSimpleExpr, ",") <~ "]" ^^ (ev.sequence(_))

  lazy val pTuple: PP[Tuple] = "(" ~> rep1sep(pSimpleExpr, ",") <~ ")" ^^ (ev.sequence(_))

  ////
  // NUMERIC

  lazy val pNumExpr: PP[NumExpr] =
    pCondNumExpr |
      pNumExpr6

  lazy val pCondNumExpr: PP[NumExpr] =
    ("if" ~> pLogicExpr <~ "then") ~ (pNumExpr6 | pNumExpr) ~ opt("else" ~> (pNumExpr6 | pNumExpr)) ^^ {
      case testS ~ ifTrueS ~ otherwiseS =>
        for {
          test <- testS
          ifTrue <- ifTrueS
          otherwise <- otherwiseS.sequence
        } yield {
          CondNumExpr(test, ifTrue, otherwise): NumExpr
        }
    }

  lazy val pNumExpr6: PP[NumExpr] =
    pNumExpr5 ~ rep(("+" | "-" | "less") ~ (pNumExpr5 | pNumExpr)) ^^ {
      case numS ~ listP =>
        for {
          num <- numS
          p = listP.map { case op ~ exprS => (op, exprS) }.unzip
          ops = p._1
          listS = p._2
          list <- listS.sequence.map(ops.zip(_))
        } yield {
          list.foldLeft(num) {
            case (exp, (oper, num)) => oper match {
              case "+" => NumAdd(exp, num)
              case "-" => NumSub(exp, num)
              case "less" => NumLess(exp, num)
              case _ => sys.error(s"""Found `$oper' expecting `("+" | "-" | "less")' on pre-validated String. This is a bug.""")
            }
          }
        }
    }

  lazy val pNumExpr5: PP[NumExpr] =
    pIterNumExpr |
      pNumExpr4

  lazy val pIterNumExpr: PP[NumExpr] =
    ("sum" | "prod" | "min" | "max") ~ pIndExpr ~ (pNumExpr4 | pNumExpr) ^^ {
      case op ~ indexingS ~ integrandS =>
        for {
          p <- scoped(indexingS, integrandS)
          indexing = p._1
          integrand = p._2
        } yield {
          (op match {
            case "sum" => NumSum(indexing, integrand)
            case "prod" => NumProd(indexing, integrand)
            case "min" => NumMin(indexing, integrand)
            case "max" => NumMax(indexing, integrand)
            case _ => sys.error(s"""Found `$op' expecting `("sum" | "prod" | "min" | "max")' on pre-validated String. This is a bug.""")
          }): NumExpr
        }
    }

  lazy val pNumExpr4: PP[NumExpr] =
    pNumExpr3 ~ rep(("*" | "/" | "div" | "mod") ~ (pNumExpr3 | pNumExpr)) ^^ {
      case numS ~ listP =>
        for {
          num <- numS
          p = listP.map { case op ~ exprS => (op, exprS) }.unzip
          ops = p._1
          listS = p._2
          list <- listS.sequence.map(ops.zip(_))
        } yield {
          list.foldLeft(num) {
            case (exp, (oper, num)) => oper match {
              case "*" => NumMult(exp, num)
              case "/" => NumDiv(exp, num)
              case "div" => NumDivExact(exp, num)
              case "mod" => NumMod(exp, num)
              case _ => sys.error(s"""Found `$oper' expecting `("*" | "/" | "div" | "mod")' on pre-validated String. This is a bug.""")
            }
          }
        }
    }

  lazy val pNumExpr3: PP[NumExpr] =
    pNumExpr2 |
      ("+" | "-") ~ (pNumExpr2 | pNumExpr) ^^ {
        case op ~ exprS =>
          for {
            exp <- exprS
          } yield {
            (op match {
              case "+" => NumUnaryPlus(exp)
              case "-" => NumUnaryMinus(exp)
              case _ => sys.error(s"""Found `$op' expecting `("+" | "-")' on pre-validated String. This is a bug.""")
            }): NumExpr
          }
      }

  lazy val pNumExpr2: PP[NumExpr] =
    pNumExpr1 ~ rep(("**" | "^") ~> (pNumExpr1 | pNumExpr)) ^^ {
      case numS ~ listS =>
        for {
          num <- numS
          list <- ev.sequence(listS)
        } yield {
          (num :: list).reduceRight((num, exp) => NumRaise(num, exp))
        }
    }

  lazy val pNumExpr1: PP[NumExpr] =
    pNumRef |
      pNumFuncRef |
      pNumLit.map(x => ev.state(x: NumExpr)) |
      "(" ~> pNumExpr <~ ")"

  lazy val pNumRef: PP[NumExpr] = pSubscriptedParamRef.as[NumExpr] | pSimpleRef.as[NumExpr]

  lazy val pNumFuncRef: PP[NumExpr] =
    "abs" ~> "(" ~> pNumExpr <~ ")" ^^ (_.map(Abs(_): NumExpr)) |
      "atan" ~> "(" ~> pNumExpr <~ ")" ^^ (_.map(Atan(_): NumExpr)) |
      "atan" ~> "(" ~> (pNumExpr ~ "," ~ pNumExpr) <~ ")" ^^ { case xS ~ _ ~ yS => for { x <- xS; y <- yS } yield Atan2(x, y): NumExpr } |
      "card" ~> "(" ~> pSetExpr <~ ")" ^^ (_.map(Card(_): NumExpr)) |
      "ceil" ~> "(" ~> pNumExpr <~ ")" ^^ (_.map(Ceil(_): NumExpr)) |
      "cos" ~> "(" ~> pNumExpr <~ ")" ^^ (_.map(Cos(_): NumExpr)) |
      "exp" ~> "(" ~> pNumExpr <~ ")" ^^ (_.map(Exp(_): NumExpr)) |
      "floor" ~> "(" ~> pNumExpr <~ ")" ^^ (_.map(Floor(_): NumExpr)) |
      "gmtime" ~ "(" ~ ")" ^^^ (ev.state(Gmtime(): NumExpr)) |
      "length" ~> "(" ~> pSymExpr <~ ")" ^^ (_.map(Length(_): NumExpr)) |
      "log" ~> "(" ~> pNumExpr <~ ")" ^^ (_.map(Log(_): NumExpr)) |
      "log10" ~> "(" ~> pNumExpr <~ ")" ^^ (_.map(Log10(_): NumExpr)) |
      "max" ~> "(" ~> rep1sep(pNumExpr, ",") <~ ")" ^^ (xS => for { x <- ev.sequence(xS) } yield Max(x: _*): NumExpr) |
      "min" ~> "(" ~> rep1sep(pNumExpr, ",") <~ ")" ^^ (xS => for { x <- ev.sequence(xS) } yield Min(x: _*): NumExpr) |
      "round" ~> "(" ~> pNumExpr ~ opt("," ~> pNumExpr) <~ ")" ^^ { case xS ~ yS => for { x <- xS; y <- ev.sequence(yS) } yield Round(x, y): NumExpr } |
      "sin" ~> "(" ~> pNumExpr <~ ")" ^^ (_.map(Sin(_): NumExpr)) |
      "sqrt" ~> "(" ~> pNumExpr <~ ")" ^^ (_.map(Sqrt(_): NumExpr)) |
      "str2time" ~> "(" ~> pSymExpr ~ "," ~ pSymExpr <~ ")" ^^ { case xS ~ _ ~ yS => for { x <- xS; y <- yS } yield Str2time(x, y): NumExpr } |
      "trunc" ~> "(" ~> pNumExpr ~ opt("," ~> pNumExpr) <~ ")" ^^ { case xS ~ yS => for { x <- xS; y <- ev.sequence(yS) } yield Trunc(x, y): NumExpr } |
      "Irand224" ~ "(" ~ ")" ^^^ (ev.state(Irand224(): NumExpr)) |
      "Uniform01" ~ "(" ~ ")" ^^^ (ev.state(Uniform01(): NumExpr))

  /**
   * MathProg numeric literal are very similar to Java BigDecimal string representation.
   *
   * One deviation from MathProg spec is that we support `d`, `D` also as characters for exponent
   * part.
   * The other deviation is that numbers with decimal separator but without decimal part are not
   * supported. So, `56.E+5` will fail. This is the same behavior as Scala numeric literals.
   */
  def pNumLit: Parser[NumLit] =
    """([+-]?)(\d+(\.\d+)?|\d*\.\d+)([dDeE][+-]?\d+)?""".r ^^ { str =>
      NumLit(BigDecimal(str.replaceAll("[dD]", "E")))
    }

  ////
  // SYMBOLIC

  lazy val pSymExpr: PP[SymExpr] =
    pCondSymExpr |
      pSymExpr8

  lazy val pCondSymExpr: PP[SymExpr] =
    ("if" ~> pLogicExpr <~ "then") ~ (pSymExpr) ~ opt("else" ~> (pSymExpr)) ^^ {
      case testS ~ ifTrueS ~ otherwiseS =>
        for {
          test <- testS
          ifTrue <- ifTrueS
          otherwise <- otherwiseS.sequence
        } yield {
          CondSymExpr(test, ifTrue, otherwise): SymExpr
        }
    }

  lazy val pSymExpr8: PP[SymExpr] =
    pSymExpr1 ~ rep("&" ~> (pSymExpr1 | pSymExpr)) ^^ {
      case symS ~ listS =>
        for {
          sym <- symS
          list <- listS.sequence
        } yield {
          (sym :: list).reduceLeft((exp, sym) => Concat(exp, sym))
        }
    }

  lazy val pSymExpr1: PP[SymExpr] =
    pSymRef |
      pSymFuncRef |
      pStringLit.map(ev.state(_: SymExpr)) |
      pNumExpr ^^ (_.map(SymNumExpr(_): SymExpr)) |
      "(" ~> pSymExpr <~ ")"

  lazy val pSymRef: PP[SymExpr] = pSubscriptedParamRef.as[SymExpr] | pSimpleRef.as[SymExpr]

  lazy val pSymFuncRef: PP[SymExpr] =
    "substr" ~> "(" ~> (pSymExpr ~ "," ~ pNumExpr ~ opt("," ~> pNumExpr)) <~ ")" ^^ {
      case expS ~ _ ~ fromS ~ lengthS => for { exp <- expS; from <- fromS; length <- lengthS.sequence } yield Substr(exp, from, length): SymExpr
    } |
      "time2str" ~> "(" ~> pNumExpr ~ "," ~ pSymExpr <~ ")" ^^ { case tS ~ _ ~ fS => for { t <- tS; f <- fS } yield Time2str(t, f): SymExpr }

  /**
   * Mathrog literals are strings delimited either by single or double quotes.
   * The delimiting character must be doubled if it appears within the literals.
   */
  val DQ = "\""
  val SQ = "'"

  def pStringLit(q: String): Parser[StringLit] =
    """x([^x]|xx)*x""".replaceAll("x", q).r ^^ {
      case lit =>
        val text = lit.stripPrefix(q).stripSuffix(q).replaceAll(q + q, q)
        StringLit(text)
    }

  lazy val pStringLit: PackratParser[StringLit] = pStringLit(DQ) | pStringLit(SQ)

  ////
  // SET

  lazy val pSetExpr: PP[SetExpr] =
    pCondSetExpr |
      pSetExpr13

  lazy val pCondSetExpr: PP[SetExpr] =
    ("if" ~> pLogicExpr <~ "then") ~ (pSetExpr13 | pSetExpr) ~ ("else" ~> (pSetExpr13 | pSetExpr)) ^^ {
      case testS ~ ifTrueS ~ otherwiseS => for { test <- testS; ifTrue <- ifTrueS; otherwise <- otherwiseS } yield CondSetExpr(test, ifTrue, otherwise): SetExpr
    }

  lazy val pSetExpr13: PP[SetExpr] =
    pSetExpr12 ~ rep(("union" | "diff" | "symdiff") ~ (pSetExpr12 | pSetExpr)) ^^ {
      case setS ~ listP =>
        for {
          set <- setS
          p = listP.map { case op ~ exprS => (op, exprS) }.unzip
          ops = p._1
          listS = p._2
          list <- listS.sequence.map(ops.zip(_))
        } yield {
          list.foldLeft(set) {
            case (exp, (oper, set)) => oper match {
              case "union" => Union(exp, set)
              case "diff" => Diff(exp, set)
              case "symdiff" => SymDiff(exp, set)
              case _ => sys.error(s"""Found `$oper' expecting `("union" | "diff" | "symdiff")' on pre-validated String. This is a bug.""")
            }
          }
        }
    }

  lazy val pSetExpr12: PP[SetExpr] =
    pSetExpr11 ~ rep("inter" ~> (pSetExpr11 | pSetExpr)) ^^ {
      case setS ~ listS =>
        for {
          set <- setS
          list <- listS.sequence
        } yield {
          list.foldLeft(set) {
            case (exp, set) => Inter(exp, set)
          }
        }
    }

  lazy val pSetExpr11: PP[SetExpr] =
    pSetExpr10 ~ rep("cross" ~> (pSetExpr10 | pSetExpr)) ^^ {
      case setS ~ listS =>
        for {
          set <- setS
          list <- listS.sequence
        } yield {
          list.foldLeft(set) {
            case (exp, set) => Cross(exp, set)
          }
        }
    }

  lazy val pSetExpr10: PP[SetExpr] =
    pIterSetExpr | pArithSet.as[SetExpr] | pSetExpr1

  lazy val pIterSetExpr: PP[SetExpr] =
    "setof" ~> pIndExpr ~ pTuple ^^ {
      case indexingS ~ integrandS =>
        for {
          p <- scoped(indexingS, integrandS)
          indexing = p._1
          integrand = p._2
        } yield SetOf(indexing, integrand): SetExpr
    } |
      "setof" ~> pIndExpr ~ pSimpleExpr ^^ {
        case indexingS ~ integrandS =>
          for {
            p <- scoped(indexingS, integrandS)
            indexing = p._1
            integrand = p._2
          } yield SetOf(indexing, List(integrand)): SetExpr
      } |
      pIndExpr ^^ {
        case indexingS =>
          for {
            _ <- putScope
            indexing <- indexingS
            _ <- removeScope
          } yield {
            IndExprSet(indexing): SetExpr
          }
      }

  lazy val pArithSet: PP[ArithSet] =
    pNumExpr ~ ".." ~ pNumExpr ~ opt("by" ~> pNumExpr) ^^ {
      case t0S ~ _ ~ tfS ~ deltaTS => for { t0 <- t0S; tf <- tfS; deltaT <- deltaTS.sequence } yield ArithSet(t0, tf, deltaT)
    }

  lazy val pSetExpr1: PP[SetExpr] =
    pSetRef.as[SetExpr] |
      pSetLit.as[SetExpr] |
      "(" ~> pSetExpr <~ ")"

  lazy val pSetRef: PP[SetRef] = pSubscriptedSetRef | pUnsubscriptedSetRef

  lazy val pSubscriptedSetRef: PP[SetRef] = wpos(pRefSymName) ~ pSubscript ^^ {
    case (name, pos) ~ subscriptS =>
      for {
        param <- ref[SetStat](name, pos)
        subscript <- subscriptS
      } yield {
        SetRef(param, subscript)
      }
  }

  lazy val pUnsubscriptedSetRef: PP[SetRef] = wpos(pRefSymName) ^^ {
    case (name, pos) =>
      for {
        param <- ref[SetStat](name, pos)
      } yield {
        SetRef(param)
      }
  }

  lazy val pSetLit: PP[SetLit] =
    "{" ~> repsep(pTuple, ",") <~ "}" ^^ (_.sequence.map(x => SetLit(x: _*))) |
      "{" ~> repsep(pSimpleExpr, ",") <~ "}" ^^ (_.sequence.map(list => SetLit(list.map(List(_)): _*)))

  // ideally this should be something in [1,20] instead of an Int
  def pDimen: Parser[Int] =
    Parser { in =>
      val numLit = pNumLit(in).filterWithError(
        x => x.num >= 1 && x.num <= 20,
        "Dimension must be integer between 1 and 20, was " + _, in)

      numLit.map(_.num.toInt)
    }

  ////
  // INDEXING

  lazy val pIndExpr: PP[IndExpr] =
    "{" ~> rep1sep(pIndEntry, ",") ~ opt(":" ~> pLogicExpr) <~ "}" ^^ {
      case entriesS ~ predS => for { entries <- entriesS.sequence; pred <- predS.sequence } yield IndExpr(entries, pred)
    }

  lazy val pIndEntry: PP[IndEntry] =
    ("(" ~> rep1sep(pSimpleExprOrDummyIndDecl, ",") <~ ")") ~ "in" ~ pSetExpr ^^ {
      case indicesS ~ _ ~ setS =>
        for {
          indices <- indicesS.sequence
          set <- setS
        } yield {

          /*
         * Each non DummyIndDecl, including references to DummyInd declared previously, 
         * is replaced by a synthetic DummyIndDecl, and a LogicExpr equating the new DummyInd with the expression is created.
         * 
         * The conjunction of all the LogicExpr created, if any, is the optional predicate of the entry
         */

          val indicesWithExpr =
            indices.map {
              case -\/(sexpr) =>
                val d = DummyIndDecl(gen.dummy.freshName, synthetic = true)
                d -> (Eq(DummyIndRef(d), sexpr): LogicExpr).some

              case \/-(d) => d -> none
            }

          val (extIndices, exprs) = indicesWithExpr.unzip
          val predicate = exprs.flatten.toNel.map(_.foldLeft1((expr1, expr2) => Conj(expr1, expr2)))

          IndEntry(extIndices, set, predicate)

        }

    } |
      pDummyIndDecl ~ "in" ~ pSetExpr ^^ {
        case indS ~ _ ~ setS =>
          for {
            ind <- indS
            set <- setS
          } yield IndEntry(List(ind), set)
      } |
      pSetExpr ^^ (_.map(IndEntry(Nil, _)))

  def pDummyIndDecl: P[DummyIndDecl] = wpos(pRefSymName) ^^ {
    case (name, pos) =>
      for {
        dummyInd <- putDecl(DummyIndDecl(name), pos)
      } yield {
        dummyInd
      }
  }

  ////
  // LOGIC

  lazy val pLogicExpr: PP[LogicExpr] =
    pLogicExpr18 ~ rep(("or" | "||") ~> (pLogicExpr18 | pLogicExpr)) ^^ {
      case propS ~ listS =>

        for {
          prop <- propS
          list <- ev.sequence(listS)
        } yield {

          list.foldLeft(prop) {
            case (exp, prop) => Disj(exp, prop)
          }

        }

    }

  lazy val pLogicExpr18: PP[LogicExpr] =
    pIterLogicExpr | pLogicExpr17

  lazy val pIterLogicExpr: PP[LogicExpr] =
    ("forall" | "exists") ~ pIndExpr ~ (pLogicExpr17 | pLogicExpr) ^^ {
      case op ~ indexingS ~ integrandS =>
        for {
          p <- scoped(indexingS, integrandS)
          indexing = p._1
          integrand = p._2
        } yield {
          op match {
            case "forall" => Forall(indexing, integrand): LogicExpr
            case "exists" => Exists(indexing, integrand): LogicExpr
            case _ => sys.error(s"""Found `$op' expecting `("forall" | "exists")' on pre-validated String. This is a bug.""")
          }
        }
    }

  lazy val pLogicExpr17: PP[LogicExpr] =
    pLogicExpr16 ~ rep(("and" | "&&") ~> (pLogicExpr16 | pLogicExpr)) ^^ {
      case propS ~ listS =>
        for {
          prop <- propS
          list <- ev.sequence(listS)
        } yield {

          list.foldLeft(prop) {
            case (exp, prop) => Conj(exp, prop)
          }
        }
    }

  lazy val pLogicExpr16: PP[LogicExpr] =
    pLogicExpr15 |
      ("not" | "!") ~> pLogicExpr15 ^^ (_.map(Neg(_): LogicExpr))

  lazy val pLogicExpr15: PP[LogicExpr] =
    pRelExpr |
      pLogicExpr14

  lazy val pRelExpr: PP[LogicExpr] =
    pSimpleExpr ~ "<" ~ pSimpleExpr ^^ {
      case leftS ~ _ ~ rightS => for { left <- leftS; right <- rightS } yield (LT(left, right): LogicExpr)
    } |
      pSimpleExpr ~ "<=" ~ pSimpleExpr ^^ {
        case leftS ~ _ ~ rightS => for { left <- leftS; right <- rightS } yield (LTE(left, right): LogicExpr)
      } |
      pSimpleExpr ~ ">" ~ pSimpleExpr ^^ {
        case leftS ~ _ ~ rightS => for { left <- leftS; right <- rightS } yield (GT(left, right): LogicExpr)
      } |
      pSimpleExpr ~ ">=" ~ pSimpleExpr ^^ {
        case leftS ~ _ ~ rightS => for { left <- leftS; right <- rightS } yield (GTE(left, right): LogicExpr)
      } |
      pSimpleExpr ~ ("==" | "=") ~ pSimpleExpr ^^ {
        case leftS ~ _ ~ rightS => for { left <- leftS; right <- rightS } yield (Eq(left, right): LogicExpr)
      } |
      pSimpleExpr ~ ("<>" | "!=") ~ pSimpleExpr ^^ {
        case leftS ~ _ ~ rightS => for { left <- leftS; right <- rightS } yield (NEq(left, right): LogicExpr)
      } |
      pTuple ~ ("not" | "!") ~ "in" ~ pSetExpr ^^ {
        case valuesS ~ _ ~ _ ~ setS => for { values <- valuesS; set <- setS } yield (NotIn(values, set): LogicExpr)
      } |
      pSimpleExpr ~ ("not" | "!") ~ "in" ~ pSetExpr ^^ {
        case valueS ~ _ ~ _ ~ setS => for { value <- valueS; set <- setS } yield (NotIn(List(value), set): LogicExpr)
      } |
      pTuple ~ "in" ~ pSetExpr ^^ {
        case valuesS ~ _ ~ setS => for { values <- valuesS; set <- setS } yield (In(values, set): LogicExpr)
      } |
      pSimpleExpr ~ "in" ~ pSetExpr ^^ {
        case valueS ~ _ ~ setS => for { value <- valueS; set <- setS } yield (In(List(value), set): LogicExpr)
      } |
      pSetExpr ~ ("not" | "!") ~ "within" ~ pSetExpr ^^ {
        case leftS ~ _ ~ _ ~ rightS => for { left <- leftS; right <- rightS } yield (NotWithin(left, right): LogicExpr)
      } |
      pSetExpr ~ "within" ~ pSetExpr ^^ {
        case leftS ~ _ ~ rightS => for { left <- leftS; right <- rightS } yield (Within(left, right): LogicExpr)
      }

  lazy val pLogicExpr14: PP[LogicExpr] =
    pLogicExpr1

  lazy val pLogicExpr1: PP[LogicExpr] =
    pNumExpr.as[LogicExpr] |
      "(" ~> pLogicExpr <~ ")"

  ////
  // LINEAR

  lazy val pLinExpr: PP[LinExpr] = pLinExprDisj.joinL(identity)

  lazy val pLinExprDisj: PP[LinExpr \/ NumExpr] =
    pCondLinExpr |
      pLinExpr6

  lazy val pCondLinExpr: PP[LinExpr \/ NumExpr] =
    ("if" ~> pLogicExpr <~ "then") ~ (pLinExpr6 | pLinExprDisj) ~ opt("else" ~> (pLinExpr6 | pLinExprDisj)) ^^ {
      case testS ~ ifTrueS ~ otherwiseS =>
        for {
          test <- testS
          ifTrue <- ifTrueS
          otherwise <- otherwiseS.sequence
        } yield {
          ifTrue.fold(
            { left =>
              (CondLinExpr(
                test,
                left,
                otherwise.map(_.fold(identity, identity))): LinExpr).left
            },
            { right =>
              otherwise match {
                case None => (CondNumExpr(test, right, none): NumExpr).right
                case Some(-\/(lin)) =>
                  (CondLinExpr(
                    test,
                    right,
                    lin.some): LinExpr).left
                case Some(\/-(num)) =>
                  (CondNumExpr(
                    test,
                    right,
                    num.some): NumExpr).right
              }
            })
        }
    }

  lazy val pLinExpr6: PP[LinExpr \/ NumExpr] =
    pLinExpr5 ~ rep(wpos("+" | "-" | "less") ~ (pLinExpr5 | pLinExprDisj)) ^^ {
      case numS ~ listP =>
        for {
          num <- numS
          p = listP.map { case op ~ exprS => (op, exprS) }.unzip
          ops = p._1
          listS = p._2
          list <- listS.sequence.map(ops.zip(_))
          result <- stateT {
            val newVal: V[LinExpr \/ NumExpr] =
              list.foldLeft(num.right[RefError]) {
                case (accum, (oper, expr)) => oper match {
                  case ("+", _) =>
                    (accum, expr) match {
                      case (\/-(-\/(lin)), \/-(num)) => (LinAdd(lin, num): LinExpr).left.right
                      case (\/-(\/-(num)), -\/(lin)) => (LinAdd(num, lin): LinExpr).left.right
                      case (\/-(\/-(num1)), \/-(num2)) => (NumAdd(num1, num2): NumExpr).right.right
                      case (\/-(-\/(lin1)), -\/(lin2)) => (LinAdd(lin1, lin2): LinExpr).left.right
                      case (x, _) => x
                    }
                  case ("-", _) =>
                    (accum, expr) match {
                      case (\/-(-\/(lin)), \/-(num)) => (LinSub(lin, num): LinExpr).left.right
                      case (\/-(\/-(num)), -\/(lin)) => (LinSub(num, lin): LinExpr).left.right
                      case (\/-(\/-(num1)), \/-(num2)) => (NumSub(num1, num2): NumExpr).right.right
                      case (\/-(-\/(lin1)), -\/(lin2)) => (LinSub(lin1, lin2): LinExpr).left.right
                      case (x, _) => x
                    }
                  case ("less", pos) =>
                    (accum, expr) match {
                      case (\/-(\/-(num1)), \/-(num2)) => (NumLess(num1, num2): NumExpr).right.right
                      case (\/-(x), y) =>
                        val str = x.fold(_.shows, _.shows) + " less " + y.fold(_.shows, _.shows)
                        TypeMismatch(s"`less' of linear expressions is not allowed: `$str'", pos).left
                      case (x, _) => x
                    }
                  case _ => sys.error(s"""Found `$oper' expecting `("+" | "-" | "less")' on pre-validated String. This is a bug.""")
                }
              }
            newVal
          }
        } yield {
          result
        }
    }

  lazy val pLinExpr5: PP[LinExpr \/ NumExpr] =
    pIterLinExpr |
      pLinExpr4

  lazy val pIterLinExpr: PP[LinExpr \/ NumExpr] =
    ("sum" | "prod" | "min" | "max") ~ pIndExpr ~ wpos(pLinExpr4 | pLinExprDisj) ^^ {
      case op ~ indexingS ~ ((integrandS, pos)) =>
        for {
          p <- scoped(indexingS, integrandS)
          indexing = p._1
          integrand = p._2
          safeIntegrand <- stateT {
            (op, integrand) match {
              case ("sum", _) => integrand.right
              case (_, \/-(num @_)) => integrand.right
              case (_, -\/(lin)) =>
                val linStr = lin.shows
                TypeMismatch(s"`$op' is not allowed for linear expresions: `$linStr'", pos).left
            }
          }
        } yield {
          ((op, safeIntegrand): @unchecked) match {
            case ("sum", _) =>
              integrand.bimap(
                LinSum(indexing, _): LinExpr,
                NumSum(indexing, _): NumExpr)
            case ("prod", \/-(num)) => (NumProd(indexing, num): NumExpr).right
            case ("min", \/-(num)) => (NumMin(indexing, num): NumExpr).right
            case ("max", \/-(num)) => (NumMax(indexing, num): NumExpr).right
          }
        }
    }

  lazy val pLinExpr4: PP[LinExpr \/ NumExpr] =
    pLinExpr3 ~ rep(wpos("*" | "/" | "div" | "mod") ~ (pLinExpr3 | pLinExprDisj)) ^^ {
      case numS ~ listP =>
        for {
          num <- numS
          p = listP.map { case op ~ exprS => (op, exprS) }.unzip
          ops = p._1
          listS = p._2
          list <- listS.sequence.map(ops.zip(_))
          result <- stateT {
            val newVal: V[LinExpr \/ NumExpr] =
              list.foldLeft(num.right[RefError]) {
                case (accum, (oper, expr)) => oper match {
                  case ("*", pos) =>
                    (accum, expr) match {
                      case (\/-(-\/(lin)), \/-(num)) => (LinMult(num, lin): LinExpr).left.right
                      case (\/-(\/-(num)), -\/(lin)) => (LinMult(num, lin): LinExpr).left.right
                      case (\/-(\/-(num1)), \/-(num2)) => (NumMult(num1, num2): NumExpr).right.right
                      case (\/-(-\/(lin1)), -\/(lin2)) =>
                        val str = lin1.shows + " * " + lin2.shows
                        TypeMismatch(s"Multiplication of linear expressions is not allowed: `$str'", pos).left
                      case (x, _) => x
                    }
                  case ("/", pos) =>
                    (accum, expr) match {
                      case (\/-(-\/(lin)), \/-(num)) => (LinDiv(lin, num): LinExpr).left.right
                      case (\/-(\/-(num1)), \/-(num2)) => (NumDiv(num1, num2): NumExpr).right.right
                      case (\/-(x), -\/(lin2)) =>
                        val str = x.fold(_.shows, _.shows) + " / " + lin2.shows
                        TypeMismatch(s"Division by linear expressions is not allowed: `$str'", pos).left
                      case (x, _) => x
                    }
                  case ("div", pos) =>
                    (accum, expr) match {
                      case (\/-(\/-(num1)), \/-(num2)) => (NumDivExact(num1, num2): NumExpr).right.right
                      case (\/-(x), y) =>
                        val str = x.fold(_.shows, _.shows) + " div " + y.fold(_.shows, _.shows)
                        TypeMismatch(s"Exact division of linear expressions is not allowed: `$str'", pos).left
                      case (x, _) => x
                    }
                  case ("mod", pos) =>
                    (accum, expr) match {
                      case (\/-(\/-(num1)), \/-(num2)) => (NumMod(num1, num2): NumExpr).right.right
                      case (\/-(x), y) =>
                        val str = x.fold(_.shows, _.shows) + " div " + y.fold(_.shows, _.shows)
                        TypeMismatch(s"Modulus of linear expressions is not allowed: `$str'", pos).left
                      case (x, _) => x
                    }
                  case _ => sys.error(s"""Found `$oper' expecting `("*" | "/" | "div" | "mod")' on pre-validated String. This is a bug.""")
                }
              }
            newVal
          }
        } yield {
          result
        }
    }

  lazy val pLinExpr3: PP[LinExpr \/ NumExpr] =
    pLinExpr2 |
      ("+" | "-") ~ (pLinExpr2 | pLinExprDisj) ^^ {
        case op ~ exprS =>
          for {
            exp <- exprS
          } yield {
            op match {
              case "+" => exp.bimap(LinUnaryPlus(_): LinExpr, NumUnaryPlus(_): NumExpr)
              case "-" => exp.bimap(LinUnaryMinus(_): LinExpr, NumUnaryMinus(_): NumExpr)
              case _ => sys.error(s"""Found `$op' expecting `("+" | "-")' on pre-validated String. This is a bug.""")
            }
          }
      }

  lazy val pLinExpr2: PP[LinExpr \/ NumExpr] =
    wpos(pLinExpr1) ~ rep(wpos("**" | "^") ~ (pLinExpr1 | pLinExprDisj)) ^^ {
      case (numS, pos) ~ listP =>
        for {
          num <- numS
          p = listP.map { case op ~ exprS => (op, exprS) }.unzip
          ops = p._1
          listS = p._2
          list <- listS.sequence.map(ops.zip(_))
          result <- stateT {
            val aux = (num, pos) :: list.map { case ((_, pos), x) => x -> pos }
            aux.init.foldRight(aux.last._1.right[RefError]) {
              case ((\/-(num1), _), \/-(\/-(num2))) => (NumRaise(num1, num2): NumExpr).right.right
              case ((x, pos), \/-(y)) =>
                val str = x.fold(_.shows, _.shows) + " ** " + y.fold(_.shows, _.shows)
                TypeMismatch(s"`raise' is not allowed for linear expresions: `$str'", pos).left
              case (_, x) => x
            }
          }
        } yield {
          result
        }
    }

  lazy val pLinExpr1: PP[LinExpr \/ NumExpr] =
    pLinRef |
      pNumFuncRef.asRight[LinExpr] |
      pNumLit.map(x => ev.state(x: NumExpr)).asRight[LinExpr] |
      "(" ~> pLinExprDisj <~ ")"

  lazy val pLinRef: PP[LinExpr \/ NumExpr] = pSubscriptedLinRef | pUnsubscriptedLinRef

  lazy val pSubscriptedLinRef: PP[LinExpr \/ NumExpr] = wpos(pRefSymName) ~ pSubscript ^^ {
    case (name, pos) ~ subscriptS =>
      for {
        subscript <- subscriptS
        linRef <- ref(name, pos, {
          case xvar: VarStat => (VarRef(xvar, subscript): LinExpr).left
          case param: ParamStat => (ParamRef(param, subscript): NumExpr).right
        })
      } yield linRef
  }

  lazy val pUnsubscriptedLinRef: PP[LinExpr \/ NumExpr] = wpos(pRefSymName) ^^ {
    case (name, pos) =>
      for {
        linRef <- ref(name, pos, {
          case xvar: VarStat => (VarRef(xvar): LinExpr).left
          case param: ParamStat => (ParamRef(param): NumExpr).right
          case dummy: DummyIndDecl => (DummyIndRef(dummy): NumExpr).right
        })
      } yield linRef
  }

  ////
  // BASIC

  def pRefSymName: Parser[SymName] = pSymName <~ not("{" | "(")

  /**
   * MathProg symbolic names can't be reserved keywords
   */
  def pSymName: Parser[SymName] = Parser { in =>
    val pSimpleSymName = """[A-Za-z_][A-Za-z0-9_]*""".r
    val resSymName = pSimpleSymName(in)
    val resResKeyw = pReservedKeyword(in)

    (resSymName, resResKeyw) match {
      case (Success(_, nextSN), Success(_, nextRK)) if nextSN.offset == nextRK.offset =>
        Failure("Reserved keywords can't be used as symbolic names", nextSN)

      case _ => resSymName
    }
  }

  def pReservedKeyword: Parser[String] =
    ("and" | "else" | "mod" | "union" | "by" | "if" | "not" | "within" | "cross" | "in" |
      "or" | "diff" | "inter" | "symdiff" | "div" | "less" | "then")

  ////

  implicit class PAOps[A](x: P[A]) {
    def as[B >: A]: P[B] = x.map(_.map(x => x: B))

    def asRight[B]: P[B \/ A] = x.map(_.map(_.right[B]))

    def asLeft[B]: P[A \/ B] = x.map(_.map(_.left[B]))
  }

  implicit class PPAOps[A](x: PP[A]) {
    def as[B >: A]: PP[B] = x.map(_.map(x => x: B))

    def asRight[B]: PP[B \/ A] = x.map(_.map(_.right[B]))

    def asLeft[B]: PP[A \/ B] = x.map(_.map(_.left[B]))
  }

  implicit class PPAOrBOps[A, B](x: PP[A \/ B]) {
    def joinL(r: B => A): PP[A] = x.map(_.map(_.fold(identity, r)))
    def joinR(l: A => B): PP[B] = x.map(_.map(_.fold(l, identity)))
  }

  ////

  /**
   * Default handling of whitespace assumes it is a regex.
   * This is an attempt to use a Parser.
   */
  override protected def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int = {
    if (skipWhitespace) {
      val w = WhitespaceParser
      w.parse(w.pWhiteSpace, source.subSequence(offset, source.length)) match {
        case w.Success(tok, _) =>
          //println(s"$source has whitespace from $offset to ${tok.length}")
          offset + tok.mkString("").length
        case _: w.NoSuccess => 
          offset
      }
    } else
      offset
  }

  ////

  def wpos[T](p: => Parser[T]): Parser[(T, Position)] = {
    val pp = Parser { in =>
      p(in) match {
        case Success(t, in1) => Success(t -> in.pos, in1)
        case ns: NoSuccess => ns
      }
    }
    new Parser[(T, Position)] {
      def apply(in: Input) = {
        val offset = in.offset
        val start = handleWhiteSpace(in.source, offset)
        pp(in.drop(start - offset))
      }
    }
  }

  ////

  def parse[T](p: Parser[T], in: String): ParseResult[T] = parse(p, new FastCharSequence(in))

  def parseModel(model: String): ParseResult[S[Model]] = parse(phrase(pModel), model)

}

object Parser {

  def apply(): Parser = new Parser

  type V[A] = RefError \/ A

  type S[A] = StateT[V, SymTab, A]

  val ev = StateT.stateTMonadState[SymTab, V]

  case class SymTab(tab: LinkedMap[SymName, Decl], upperScope: Option[SymTab] = None) {

    def +(keyValue: (SymName, Decl)): SymTab = copy(tab = tab + keyValue)

    def +(d: Decl): SymTab = copy(tab = tab + (d.name -> d))

    def -(key: SymName): SymTab = copy(tab = tab - key)

    def -(d: Decl): SymTab = copy(tab = tab - d.name)

    def get(key: SymName): Option[Decl] = {
      val currScope = tab.get(key)
      currScope.orElse(upperScope.flatMap(_.get(key)))
    }

    def contains(name: SymName): Boolean = get(name).isDefined

    def string(size: Int): String = {
      val ws = " " * size
      s"""SymTab(
         |$ws  tab=${tab.map(x => s"$ws    ${x._1} -> ${x._2}").mkString("\n", "\n", "")}
         |$ws  upperScope=${upperScope.map(_.string(size + 2)) | "none"}
         |$ws)""".stripMargin
    }

    override def toString = string(0)

  }

  object SymTab {
    def empty: SymTab = SymTab(LinkedMap.empty)
  }

  ////

  object WhitespaceParser extends RegexParsers {

    override def skipWhitespace = false

    def pChrExcept(cs: Char*): Parser[Char] = elem("", ch => (cs forall (ch != _)))

    /**
     * MathProg line comments begin with # and extends to the end of the current line
     */
    private def pLineComment: Parser[String] =
      "#" ~ rep(pChrExcept(EofCh, '\n')) ^^ { case n ~ l => n + l.mkString("") }

    /**
     * MathProg multi-line comments are delimited by /* and */, and do not nest
     */
    private def pBlockComment: Parser[String] =
      "/*" ~ pBlockCommentClose ^^ { case s ~ l => s + l }

    private def pBlockCommentClose: Parser[String] =
      "*/" | pChrExcept(EofCh) ~ pBlockCommentClose ^^ { case a ~ b => a + b }

    /**
     * debug
     */
    def p[T](name: String)(x: T): T = { println(name); x }

    def pWhiteSpace: Parser[List[String]] = rep(pLineComment | pBlockComment | whiteSpace)

  }

  ////

  def stateT[A](f: => V[A]): S[A] = StateT[V, SymTab, A](symTab => f.map(symTab -> _))

  def stateT[A](f: SymTab => V[(SymTab, A)]): S[A] = StateT[V, SymTab, A](f)

  def putDecl[A <: Decl](a: A, pos: Position): S[A] = stateT { symTab =>
    symTab.get(a.name).fold {
      val newState = symTab + (a.name -> a)
      (newState -> a).right[RefError]
    } { decl =>
      AlreadyDeclared(a.name, decl, pos).left
    }
  }

  def updateDecl[A <: Decl](a: A): S[A] = stateT { symTab =>
    val newState = symTab + (a.name -> a)
    (newState -> a).right
  }

  def removeScope: S[SymTab] = stateT { symTab =>
    val newState = symTab.upperScope | SymTab.empty
    (newState -> symTab).right
  }

  def putScope: S[SymTab] = stateT { symTab =>
    val newState = SymTab(LinkedMap.empty, symTab.some)
    (newState -> symTab).right
  }

  def scoped[A, B](indexingS: S[A], integrandS: S[B]): S[(A, B)] =
    for {
      _ <- putScope
      indexing <- indexingS
      integrand <- integrandS
      _ <- removeScope
    } yield {
      //println(s)
      indexing -> integrand
    }

  def ref[A](name: SymName, pos: Position, pf: PartialFunction[Decl, A])(implicit Manifest: Manifest[A]): S[A] = stateT { symTab =>
    for {
      decl <- symTab.get(name).toRightDisjunction(NotDeclared(name, pos))
      a <- pf.lift(decl).toRightDisjunction(TypeMismatch(decl, Manifest, pos))
    } yield {
      (symTab, a)
    }

  }

  def ref[A <: Decl](name: SymName, pos: Position)(implicit Manifest: Manifest[A]): S[A] = stateT { symTab =>

    val pf: PartialFunction[Any, A] = { case a: A => a }

    for {
      given <- symTab.get(name).toRightDisjunction(NotDeclared(name, pos))
      a <- pf.lift(given).toRightDisjunction(TypeMismatch(given, Manifest, pos))
    } yield {
      (symTab, a)
    }

  }

  def refDisj[A <: Decl, B <: Decl](name: SymName, pos: Position)(implicit ManifestA: Manifest[A], ManifestB: Manifest[B]): S[A \/ B] = stateT { symTab =>

    val pf: PartialFunction[Any, A \/ B] = {
      case a: A => a.left
      case b: B => b.right
    }

    for {
      given <- symTab.get(name).toRightDisjunction(NotDeclared(name, pos))
      d <- pf.lift(given).toRightDisjunction(TypeMismatch(given, manifest[A \/ B], pos))
    } yield {
      (symTab, d)
    }

  }

  ////

  def posStr(x: Position): String = {
    val str = s"[$x]"
    val lines = x.longString.split("\n")
    val ts = 12
    val linesTab = for { (l, i) <- lines.zipWithIndex } yield {
      if (i == 0)
        str + " " * (ts - str.length) + l
      else " " * ts + l
    }
    s"\n${linesTab.mkString("\n")}"
  }

  sealed abstract class RefError(override val toString: String)

  case class NotDeclared(name: SymName, pos: Position) extends RefError(s"`$name' not declared.${posStr(pos)}")

  case class AlreadyDeclared(name: SymName, decl: Decl, pos: Position) extends RefError(
    s"`$name' already declared as `${decl.getClass.getSimpleName} $name'.${posStr(pos)}")

  case class TypeMismatch(text: String, pos: Position) extends RefError(text + posStr(pos))

  object TypeMismatch {
    def apply[A: Manifest](given: A, name: String, expectedType: Manifest[_], pos: Position): TypeMismatch = apply(
      s"`${name}' hast incorrect type. " +
        s"Expected `${string(expectedType)}', " +
        s"found `${given.getClass.getSimpleName}'", pos)

    def apply[A <: Decl: Manifest](given: A, expectedType: Manifest[_], pos: Position): TypeMismatch = {
      apply(given, given.name, expectedType, pos)
    }

    def string[A](m: Manifest[A]): String = {
      val simpleName = scala.reflect.NameTransformer.decode(m.runtimeClass.getSimpleName)
      m.typeArguments match {
        case Nil => simpleName
        case xs => s"$simpleName[${xs.map(string(_)).mkString(", ")}]"
      }
    }
  }

  ////

  object Implicits {

    implicit class RunParseResult[A](result: Parsers#ParseResult[S[A]]) {

      import scalaz.{ Success => _, _ } //, Scalaz._

      def run(implicit symTab: SymTab): \/[Parsers#NoSuccess \/ RefError, (SymTab, A)] = {

        result match {
          case s: Parsers#Success[S[A]] =>

            s.result.run(symTab) match {
              case -\/(err) =>
                err.right.left

              case \/-(res) =>
                res.right
            }

          case e: Parsers#NoSuccess =>
            e.left.left

        }

      }

      def run2(implicit symTab: SymTab): \/[Parsers#NoSuccess \/ RefError, A] = {
        run match {
          case \/-((_, value)) =>
            //println(state); println()
            value.right

          case -\/(x) => x.left
        }

      }

      def runR(implicit symTab: SymTab): A = run2 match {
        case \/-(a) => a
        case -\/(err) => sys.error(s"Right value expected. Left found: $err")
      }

    }

  }

  ////

  /**
   * https://github.com/begeric/FastParsers/blob/ab27b614e99433c568cf8545edfdc1c144ccf789/FastParsers/src/test/scala/FastCharSequence.scala
   */
  class FastCharSequence(chars: Array[Char], val startBounds: Int, val endBounds: Int) extends CharSequence {
    def this(chars: Array[Char]) = this(chars, 0, chars.length)
    def this(input: String) = this(input.toCharArray)
    def length(): Int = endBounds - startBounds
    def charAt(index: Int): Char = {
      if (index < length) {
        chars(index + startBounds)
      } else {
        throw new IndexOutOfBoundsException(s"$boundsInfo index: $index")
      }
    }
    def subSequence(start: Int, end: Int): CharSequence = {
      if (start >= 0 && start <= length && end >= 0 && end <= length) {
        new FastCharSequence(chars, startBounds + start, startBounds + end)
      } else {
        throw new IndexOutOfBoundsException(s"$boundsInfo start: $start, end $end")
      }
    }
    override def toString(): String = new String(chars, startBounds, length)
    private def boundsInfo = s"current: (startBounds: $startBounds, endBounds: $endBounds, length: $length, chars length: ${chars.length})"
  }

}
