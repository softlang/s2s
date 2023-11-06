package org.softlang.s2s.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator._

/** A parser for conjunctive G-CORE queries. */
class GCoreParser extends RegexParsers:
  import org.softlang.s2s.query.GCORE._

  def apply(input: String): Query = parseAll(pBasicGraphQuery, input) match
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error("Parser error: " + failure.msg)

  // Queries

  def pBasicGraphQuery: Parser[BasicGraphQuery] = 
    pConstructClause ~ pMatchClause ^^ {
      case c ~ m => (c,m)
    }

  def pConstructClause: Parser[ConstructClause] = 
    "CONSTRUCT" ~> repsep(pBasicGraphPattern, ",") ^^ {
      case f => ConstructClause.Construct(f.toSet)
    }

  def pMatchClause: Parser[MatchClause] =
    "MATCH" ~> repsep(pBasicGraphPattern, ",") ~ opt("WHERE" ~> pWhenClause) ^^ { 
      case f ~ Some(w) => MatchClause.MatchWhere(f.toSet, w)
      case f ~ None => MatchClause.Match(f.toSet)
    }

  // Conditional clauses.

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
