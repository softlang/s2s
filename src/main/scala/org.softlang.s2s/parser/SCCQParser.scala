package org.softlang.s2s.parser

import org.softlang.s2s.core._
import org.softlang.s2s.query.SCCQ

import de.pseifer.shar.Shar

/** Parse a SCCQ. */
class SCCQParser(shar: Shar):

  private object QP extends SPARQLParser:
    def parse(in: String): S2STry[SCCQ] =
      parse(Query, in) match {
        case Success(q, _) =>
          q.toQuery(shar) match {
            case Right(qi) =>
              for
                c <- Util.flipEitherHead(qi._1.map(_.toAtomicPattern))
                w <- Util.flipEitherHead(qi._2.map(_.toAtomicPattern))
              yield SCCQ(c, w)
            case Left(msg) => Left(UnparseableQueryError(msg.show))
          }
        case Failure(msg, _) => Left(UnparseableQueryError(msg))
        case Error(msg, _)   => Left(UnparseableQueryError(msg))
      }

  /** Parse a SCCQ query. */
  def parse(in: String): S2STry[SCCQ] = QP.parse(in)
