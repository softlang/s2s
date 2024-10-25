package org.softlang.s2s.main

import org.softlang.s2s.core.S2STry
import org.softlang.s2s.query.GCORE
import org.softlang.s2s.parser.GCOREParser
import org.softlang.s2s.core.Util

case class ServerRoutes()(implicit cc: castor.Context,
                           log: cask.Logger) extends cask.Routes{

  @cask.postJson("/parse")
  def jsonEndpointObj(query: ujson.Str) = {
    val result = Implementation.parse(query._1)

    println("\nHandling request:")
    println(query)
    println(result)

    if result.isEmpty then
      ujson.Obj(
        "valid" -> true,
        "error" -> ""
      )
    else
      ujson.Obj(
        "valid" -> false,
        "error" -> result.getOrElse("")
      )
  }

  @cask.postJson("/type")
  def jsonEndpointObj(query: ujson.Str, iri: ujson.Str, args: ujson.Obj) = {

    println("\nHandling request:")
    println(query)
    println(iri)
    println(args)

    // Response: Sequence of objects, outlining the type of result.
    ujson.Obj(
      "result" -> Seq(
        ujson.Obj(
            // Kind: node, edge, value.
            "kind" -> "node",
            // For node or edge: properties and types.
            "props" -> "age:int;name:str",
            // For node or edge: labels
            "labels" -> "Person",
          )
       ),
      // If error != null, then "result" is null.
      "error" -> "",
    )
  }

  initialize()
}

object Server extends cask.Main{
  val allRoutes = Seq(ServerRoutes())
}

object Implementation:

  /** Separator in query composition. */
  val SEPARATOR = "<<>>"

  /** Parse a given composition of queries. */
  private def parseInternal(src: String): S2STry[List[GCORE]] =
    val queries = src.split(SEPARATOR)
    val parser = GCOREParser()
    Util.flipEitherHead(queries.map(parser.apply(_)).toList)

  /** Parse a query and return success or parse error. */
  def parse(src: String): Option[String] =
    parseInternal(src) match {
      case Left(err) => Some(err.toString)
      case Right(_) => None
    }

  /** Infer the type of a query and check with supplied arguments. */
  def infer_type(src: String, iri: String): Option[String] =
    // TODO
    // DONE Parse the (composed) queries, including SELECT 
    // DONE -> make '{' '}' parseable; or pseudo-apply first?
    // DONE -> make SELECT queries parseable
    //
    // Apply composable knowledge construction to the composition of queries
    // -> make this work with SELECT queries
    // -> SELECT can be 
    // Infer the types for spliced-in arguments
    // Validate those with the given arguments
    // Infer the types for results
    // Construct encoded result-type dictionary
    Some("error")

end Implementation
