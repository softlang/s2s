package org.softlang.s2s.parser

import java.io.FileReader
import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer

import scala.language.postfixOps

// Builds upon https://gist.github.com/datagraph/353854

/** */
class SPARQLParser extends RegexParsers {

  // QUERY

  def Query: Parser[QueryParseResult] =
    Prologue ~ (ConstructQuery) ^^ { case prologue ~ query =>
      (prologue, query)
    }

  // PROLOGUE

  def Prologue: Parser[List[(PrefixName, ParsedIri)]] =
    (BaseDecl ?) ~ (PrefixDecl*) ^^ {
      case _ ~ Nil      => Nil
      case _ ~ prefixes => prefixes
    }

  def BaseDecl: Parser[List[Any]] =
    "BASE" ~ IRI_ref ^^ { case _ ~ iri =>
      List("BASE", iri)
    }

  def PrefixDecl: Parser[(PrefixName, ParsedIri)] =
    "PREFIX" ~ PREFIX ~ ":" ~ IRI_ref ^^ { case _ ~ ns ~ _ ~ iri =>
      (PrefixName(ns), ParsedIri(iri))
    }

  // CONSTRUCT

  def ConstructQuery: Parser[(List[Triple], List[Triple])] =
    "CONSTRUCT" ~ ConstructTemplate ~ (DatasetClause*) ~ WhereClause ^^ {
      case _ ~ t ~ _ ~ w => (t, w)
    }

  def ConstructTemplate: Parser[List[Triple]] =
    "{" ~> (ConstructTriples ?) <~ "}" ^^ {
      case None     => Nil
      case Some(ts) => ts
    }

  def ConstructTriples: Parser[List[Triple]] =
    TriplesSameSubject ~ (("." ~> (ConstructTriples ?)) ?) ^^ {
      case t ~ None           => t
      case t ~ Some(None)     => t
      case t ~ Some(Some(ts)) => t ++ ts
    }

  def TriplesSameSubject: Parser[List[Triple]] =
    (VarOrTerm ~ PropertyListNotEmpty) ^^ { case s ~ polist =>
      polist.flatMap { case (p, os) =>
        os.map { case o =>
          (s, p, o)
        }
      }
    }

  def Verb: Parser[TripleType] =
    VarOrIRIref | ARdfT

  def ARdfT: Parser[TripleType] =
    "a".r ^^ { case _ => ParsedIri("rdf:type") }

  def PropertyListNotEmpty: Parser[List[(TripleType, List[TripleType])]] =
    repsep(
      Verb ~ ObjectList ^^ { case p ~ olist => (p, olist) },
      ";"
    ) <~ (";" ?)

  def ObjectList: Parser[List[TripleType]] =
    repsep(Object, ",")

  def Object: Parser[TripleType] =
    GraphNode

  def GraphNode: Parser[TripleType] =
    VarOrTerm // | TriplesNode

  // def TriplesNode: Parser[Any] =
  //  Collection | BlankNodePropertyList

  // def Collection: Parser[Any] =
  //  "(" ~> (GraphNode +) <~ ")"

  // def BlankNodePropertyList: Parser[Any] =
  //  "[" ~> PropertyListNotEmpty <~ "]"

  def VarOrTerm: Parser[TripleType] =
    Var | GraphTerm

  def GraphTerm: Parser[TripleType] =
    IRIref // | RDFLiteral | NumericLiteral | BooleanLiteral | BlankNode | NIL
  // TODO!

  def WhereClause: Parser[List[Triple]] =
    ("WHERE" ?) ~> GroupGraphPattern

  def GroupGraphPattern: Parser[List[Triple]] =
    "{" ~> (TriplesBlock ?) <~ "}" ^^ {
      case Some(a) => a
      case None    => Nil
    }

  def TriplesBlock: Parser[List[Triple]] =
    TriplesSameSubject ~ (("." ~ (TriplesBlock ?)) ?) ^^ {
      case a ~ None              => a
      case a ~ Some(_ ~ None)    => a
      case a ~ Some(_ ~ Some(b)) => a ++ b
    }

  // QUERY BASIC

  def DatasetClause: Parser[List[Any]] =
    "FROM" ~> (DefaultGraphClause | NamedGraphClause) ^^ (List("FROM", _))

  def DefaultGraphClause: Parser[Any] = SourceSelector

  def NamedGraphClause: Parser[List[Any]] =
    "NAMED" ~> SourceSelector ^^ (List("NAMED", _))

  def SourceSelector: Parser[Any] = IRIref

  // BASIC BASIC

  def VarOrIRIref: Parser[TripleType] =
    Var | IRIref

  def IRIref: Parser[ParsedIri] =
    (IRI_ref | PrefixedName) ^^ (ParsedIri(_))

  def Var: Parser[ParsedVar] =
    VARIABLE_TOKEN ~> VARIABLE_NAME ^^ (ParsedVar(_))

  def IRI_ref: Parser[String] =
    "<" ~> IRI <~ ">" ^^ { "<" ++ _ ++ ">" }

  def PrefixedName: Parser[String] =
    PREFIX ~ ":" ~ LOCAL_NAME ^^ { case ns ~ _ ~ local =>
      ns ++ ":" ++ local
    }

  // REGEX

  private val IRI: Regex = """([^<>"{}|^`\\])*""".r

  private val VARIABLE_TOKEN: Regex = """\?|$"""".r

  private val VARIABLE_NAME: Regex = """[a-zA-Z_]\w*""".r

  private val PREFIX: Regex =
    """([a-zA-Z][a-zA-Z0-9-_.]*[a-zA-Z0-9-_])?""".r

  private val LOCAL_NAME: Regex =
    """[a-zA-Z0-9_]([a-zA-Z0-9-_.]*[a-zA-Z0-9-_])?""".r
}
