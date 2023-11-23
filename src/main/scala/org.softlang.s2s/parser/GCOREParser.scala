package org.softlang.s2s.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator._

import org.softlang.s2s.core.S2STry
import org.softlang.s2s.core.UnparsableQueryError
import org.softlang.s2s.query.GCORE

/** A parser for conjunctive G-CORE queries. */
class GCOREParser extends RegexParsers:
  import org.softlang.s2s.query.GCORE._

  def apply(input: String): S2STry[GCORE] = parseAll(pBasicGraphQuery, input) match
    case Success(result, _) => Right(GCORE(result._1, result._2))
    case failure : NoSuccess => Left(UnparsableQueryError(failure.msg))

  // Queries

  def pBasicGraphQuery: Parser[(Construct, Match)] = 
    pConstruct ~ pMatch ^^ {
      case c ~ m => (c,m)
    }

  def pConstruct: Parser[Construct] = 
    "CONSTRUCT" ~> repsep(pBasicGraphPattern, ",") ~ opt("SET" ~> pSetClauses) ~ opt("REMOVE" ~> pRemoveClauses) ^^ {
      case f ~ s ~ r => Construct(f.toSet, s.toSet.flatten, r.toSet.flatten)
    }

  def pMatch: Parser[Match] =
    "MATCH" ~> repsep(pBasicGraphPattern, ",") ~ opt("WHERE" ~> pWhenClauses) ^^ { 
      case f ~ Some(w) => Match(f.toSet, w.toSet)
      case f ~ None => Match(f.toSet, Set())
    }

  // Set and Remove clauses.

  def pSetClauses: Parser[Set[SetClause]] =
    rep1sep(pSetClause, "AND") ^^ { _.toSet }

  def pSetClause: Parser[SetClause] =
    pSetKeyValue | pSetLabel
  
  def pSetLabel: Parser[SetClause.SetLabel] =
    pVariable ~ pLabel ^^ { case x ~ k => SetClause.SetLabel(x, k) }

  def pSetKeyValue: Parser[SetClause.SetKeyValue] =
    pVariable ~ pKey ~ "=" ~  pValue ^^ { case x ~ k ~ _ ~ v => SetClause.SetKeyValue(x, k, v) }

  def pRemoveClauses: Parser[Set[RemoveClause]] =
    rep1sep(pRemoveClause, "AND") ^^ { _.toSet }

  def pRemoveClause: Parser[RemoveClause] =
    pRemoveKey| pRemoveLabel
  
  def pRemoveLabel: Parser[RemoveClause.RemoveLabel] =
    pVariable ~ pLabel ^^ { case x ~ k => RemoveClause.RemoveLabel(x, k) }

  def pRemoveKey: Parser[RemoveClause.RemoveKey] =
    pVariable ~ pKey ^^ { case x ~ k => RemoveClause.RemoveKey(x, k) }

  // Conditional clauses.

  def pWhenClauses: Parser[Set[WhenClause]] =
    rep1sep(pWhenClause, "AND") ^^ { _.toSet }

  def pWhenClause: Parser[WhenClause] =
    pHasKeyValue | pHasKey | pHasLabel
  
  def pHasKey: Parser[WhenClause.HasKey] =
    pVariable ~ pKey ^^ { case x ~ k => WhenClause.HasKey(x, k) }

  def pHasLabel: Parser[WhenClause.HasLabel] =
    pVariable ~ pLabel ^^ { case x ~ k => WhenClause.HasLabel(x, k) }

  def pHasKeyValue: Parser[WhenClause.HasKeyValue] =
    pVariable ~ pKey ~ "=" ~  pValue ^^ { case x ~ k ~ _ ~ v => WhenClause.HasKeyValue(x, k, v) }

  // Basic graph pattern.

  def pBasicGraphPattern: Parser[BasicGraphPattern] =
    pEdgePattern | pNodePattern

  def pNodePattern: Parser[BasicGraphPattern.NodePattern] =
    "(" ~> pVariable <~ ")" ^^ (BasicGraphPattern.NodePattern(_))

  def pEdgePattern: Parser[BasicGraphPattern.EdgePattern] =
    "(" ~ pVariable ~ ")-[" ~ pVariable ~ "]->(" ~ pVariable ~ ")" ^^ { case _ ~ x ~ _ ~ z ~ _ ~ y ~ _ =>
      BasicGraphPattern.EdgePattern(x, z, y)
    }

  // Values

  def pValue: Parser[Value] =
    pStringValue | pBooleanValue | pIntValue

  def pBooleanValue: Parser[Value.BooleanValue] = 
    ("true" | "false")  ^^ { case b => Value.BooleanValue(b.toBoolean) }

  def pIntValue: Parser[Value.IntValue] =
    INT ^^ { case i => Value.IntValue(i.toInt) }

  def pStringValue: Parser[Value.StringValue] =
    STRING ^^ { case s => Value.StringValue(s.drop(1).dropRight(1)) }

  // Names - variable, label, and key.

  def pVariable: Parser[Variable] =
    NAME ^^ (Variable(_))

  def pLabel: Parser[Label] =
    ":" ~> NAME ^^ (Label(_))

  def pKey: Parser[Key] =
    "." ~> NAME ^^ (Key(_))

  private val NAME: Regex = 
    """[a-zA-Z]([a-zA-Z0-9-_]*[a-zA-Z0-9-_])?""".r

  private val INT: Regex =
    """-?[0-9][0-9]*""".r

  private val STRING: Regex =
    """\"[^\"]*\"""".r
