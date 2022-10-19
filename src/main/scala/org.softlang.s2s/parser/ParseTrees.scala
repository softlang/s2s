package org.softlang.s2s.parser

import org.softlang.s2s.core.{Var, ShassTry}
import org.softlang.s2s.query.GeneralAtomicPattern
import GeneralAtomicPattern._
import de.pseifer.shar.Shar
import de.pseifer.shar.core.{Prefix, Iri}
import de.pseifer.shar.error.SharTry
import org.stringtemplate.v4.compiler.GroupParser.formalArgs_scope

sealed trait TripleType

case class ParsedVar(s: String) extends TripleType:
  def convert: Var = Var(s)

case class ParsedIri(s: String) extends TripleType:
  def convert(implicit shar: Shar): SharTry[Iri] =
    shar.state.prefixes.expandString(s)

case class PrefixName(s: String)

type Triple = (TripleType, TripleType, TripleType)

type QueryParseResult =
  (List[(PrefixName, ParsedIri)], (List[Triple], List[Triple]))

extension (p: QueryParseResult)
  def toQuery(
      shar: Shar
  ): SharTry[(List[GeneralAtomicPattern], List[GeneralAtomicPattern])] = p match
    case (prefixes, (construct, where)) =>
      // Convert to lists of general atomic pattern.
      for
        _ <- Util.addPrefixes(shar, prefixes)
        cg <- Util.toQuery(construct)(shar)
        wg <- Util.toQuery(where)(shar)
      yield (cg, wg)

private object Util:
  def isRdfType(s: String): Boolean =
    s == "rdf:type" || s == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

  def addPrefixes(
      shar: Shar,
      prefixes: List[(PrefixName, ParsedIri)]
  ): SharTry[true] =
    prefixes
      .map { case (p, i) =>
        for
          pp <- Prefix.fromString(p.s ++ ":")
          ii <- Iri.fromString(i.s)
        yield shar.state.prefixes.add(pp, ii)
      }
      .partitionMap(identity) match {
      case (Nil, rights) => Right(true)
      case (lefts, _)    => Left(lefts.head)
    }

  def toQuery(
      triples: List[Triple]
  )(implicit shar: Shar): SharTry[List[GeneralAtomicPattern]] =
    triples
      .map {

        // ***** __A_ *****
        case (
              s @ ParsedVar(_),
              p @ ParsedIri(_),
              o @ ParsedIri(_)
            ) if isRdfType(p.s) =>
          for oo <- o.convert
          yield VAC(s.convert, oo)
        case (
              s @ ParsedIri(_),
              p @ ParsedIri(_),
              o @ ParsedVar(_)
            ) if isRdfType(p.s) =>
          for ss <- s.convert
          yield LAV(ss, o.convert)
        case (
              s @ ParsedIri(_),
              p @ ParsedIri(_),
              o @ ParsedIri(_)
            ) if isRdfType(p.s) =>
          for
            ss <- s.convert
            oo <- o.convert
          yield LAC(ss, oo)
        case (
              s @ ParsedVar(_),
              p @ ParsedIri(_),
              o @ ParsedVar(_)
            ) if isRdfType(p.s) =>
          Right(VAV(s.convert, o.convert))

        // ***** __P_ *****
        case (
              s @ ParsedIri(_),
              p @ ParsedIri(_),
              o @ ParsedIri(_)
            ) =>
          for
            oo <- o.convert
            pp <- p.convert
            ss <- s.convert
          yield LPL(ss, pp, oo)
        case (
              s @ ParsedIri(_),
              p @ ParsedIri(_),
              o @ ParsedVar(_)
            ) =>
          for
            ss <- s.convert
            pp <- p.convert
          yield LPV(ss, pp, o.convert)
        case (
              s @ ParsedVar(_),
              p @ ParsedIri(_),
              o @ ParsedIri(_)
            ) =>
          for
            oo <- o.convert
            pp <- p.convert
          yield VPL(s.convert, pp, oo)
        case (
              s @ ParsedVar(_),
              p @ ParsedIri(_),
              o @ ParsedVar(_)
            ) =>
          for pp <- p.convert
          yield VPV(s.convert, pp, o.convert)

        // ***** _?P_ *****
        case (
              s @ ParsedIri(_),
              p @ ParsedVar(_),
              o @ ParsedIri(_)
            ) =>
          for
            oo <- o.convert
            ss <- s.convert
          yield LVL(ss, p.convert, oo)
        case (
              s @ ParsedIri(_),
              p @ ParsedVar(_),
              o @ ParsedVar(_)
            ) =>
          for ss <- s.convert
          yield LVV(ss, p.convert, o.convert)
        case (
              s @ ParsedVar(_),
              p @ ParsedVar(_),
              o @ ParsedIri(_)
            ) =>
          for oo <- o.convert
          yield VVL(s.convert, p.convert, oo)
        case (
              s @ ParsedVar(_),
              p @ ParsedVar(_),
              o @ ParsedVar(_)
            ) =>
          Right(VVV(s.convert, p.convert, o.convert))
      }
      .partitionMap(identity) match {
      case (Nil, rights) => Right(rights)
      case (lefts, _)    => Left(lefts.head)
    }
end Util
