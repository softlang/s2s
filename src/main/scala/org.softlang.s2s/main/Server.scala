package org.softlang.s2s.main

import org.softlang.s2s.core.Axioms
import org.softlang.s2s.core.inScope
import org.softlang.s2s.core.dropScope
import org.softlang.s2s.core.S2STry
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.infer.AlgorithmInput
import org.softlang.s2s.infer.Shapes2Shapes
import org.softlang.s2s.query.GCORE
import org.softlang.s2s.parser.GCOREParser
import org.softlang.s2s.core.Util

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Vocabulary
import org.softlang.s2s.core.Scope
import de.pseifer.shar.core.Iri
import cask.endpoints.get
import uk.ac.manchester.cs.jfact.kernel.todolist.TODOListSaveState
import scala.annotation.threadUnsafe

// ----------------------------------------------------------------------------
//                                                             Here be dragons.
// ----------------------------------------------------------------------------

// This is a prototype backend for providing type-level support,
// e.g., in programming languages. It uses a JSON API offering
// support for parsing of queries (`/parse`)  and typing of
// queries (`/type`).
//
// '/parse': Takes a {'query': <String-encoded Query>} and returns
//
// {
//  'valid': <A boolean value, signaling syntactic validity.>
//  'error': <Optional parse error message, or "" if valid.>
// }
//
// '/type': The typing endpoint takes a
//
//   {
//    'query': <String-encoded Query>,
//    'iri': <IRI identifying target graph>,
//    'args': <optional arguments for the query as a
//      [{
//        'type': <A string label, any of "int", "str" or "none" (unknown)>,
//        'literal': <A value of type String, Int, or null (no known literal)>
//      }]
//  }
//
//  The endpoint returns:
//
// {
//  'result': <Either "" if error, or a result as
//  [{
//    'kind':  <The kind of this query, one of 'node', 'edge', 'value'>,
//    'props': <Semicolon separted list of type names in "int", "str", or "none">,
//    'labels': <Semicolon separated list of labels>,
//  }]>,
//  'error': <An optional type or processing error message, or "" if valid.>
// }

enum ArgumentTypeInfo:
  case Unknown
  case AnyString
  case AnyInt
  case TheString(s: String)
  case TheInt(i: Int)

case class ServerRoutes(impl: Implementation)(implicit
    cc: castor.Context,
    log: cask.Logger
) extends cask.Routes {

  val logEnabled = true
  println("\nListening for requests on port 8080")

  @cask.postJson("/parse")
  def jsonEndpointObj(query: ujson.Str) =
    val result = impl.parse(query._1)

    if logEnabled then
      println("\n/parse")
      println(query)

    val response =
      if result.isEmpty then
        ujson.Obj(
          "valid" -> true,
          "error" -> ""
        )
      else
        println(result)
        ujson.Obj(
          "valid" -> false,
          "error" -> result.getOrElse("")
        )

    if logEnabled then
      println("->")
      println(response)

    // Return result of parsing
    response

  @cask.postJson("/type")
  def jsonEndpointObj(query: ujson.Str, iri: ujson.Str, args: ujson.Obj) =

    // Parse arguments into simple string-based Map structure.
    // Originally a mapping from argument names to dicts of shape:
    //    {"type": "int"/"str"/"none", "literal": str | int | None}
    val preparedArgs = args._1
      .map(x =>
        val tpe = x._2.obj._1("type").strOpt
        val lit = x._2.obj._1("literal").strOpt
        x._1 -> (tpe match
          case Some("str") =>
            lit match
              case Some(s) => ArgumentTypeInfo.TheString(s)
              case None    => ArgumentTypeInfo.AnyString
          case Some("int") =>
            lit match
              case Some(i) => ArgumentTypeInfo.TheInt(i.toInt) // might throw
              case None    => ArgumentTypeInfo.AnyInt
          case _ => ArgumentTypeInfo.Unknown
        )
      )
      .toMap

    val result = impl.inferType(query._1, iri._1, preparedArgs)

    if logEnabled then
      println("\n/type")
      println(query)
      println(iri)
      println(preparedArgs)

    val response = result match
      case Left(error) =>
        ujson.Obj(
          "result" -> "",
          "error" -> error
        )
      case Right(k, l, p) =>
        ujson.Obj(
          "result" -> Seq(
            ujson.Obj(
              // Kind: node, edge, value.
              "kind" -> k,
              // For node or edge: properties and types.
              "props" -> p,
              // For node or edge: labels
              "labels" -> l
            )
          ),
          "error" -> ""
        )

    if logEnabled then
      println("->")
      println(response)

    // Return resulting type information for the query.
    response

  initialize()
}

object Server extends cask.Main:
  val allRoutes = Seq(
    ServerRoutes(Implementation())
  ) // TODO: Add Config to Server for Implementation

type TypeAnnotation = (String, String, String)

class Implementation(
    val predefinedAxioms: Map[String, Axioms] = Map(
      "<https://github.com/softlang/s2s/example-graph-one>" ->
        Axioms(
          Set(
            Subsumption(
              NamedConcept(
                Iri
                  .fromString("<https://github.com/softlang/s2s/nlabel/Person>")
                  .toOption
                  .get
              ),
              NamedConcept(
                Iri
                  .fromString("<https://github.com/softlang/s2s/nlabel/Agent>")
                  .toOption
                  .get
              )
            ),
            Subsumption(
              NamedConcept(
                Iri
                  .fromString("<https://github.com/softlang/s2s/nlabel/Dog>")
                  .toOption
                  .get
              ),
              NamedConcept(
                Iri
                  .fromString("<https://github.com/softlang/s2s/nlabel/Agent>")
                  .toOption
                  .get
              )
            )
          ).map(_.inScope(Scope.Out)(Scopes.default("•"))),
          Scopes.default("•")
        )
    )
):

  /** Separator in query composition. */
  val SEPARATOR = "<<>>"

  /** Parse a given composition of queries. */
  private def parseInternal(src: String): S2STry[List[GCORE]] =
    val queries = src.split(SEPARATOR)
    val parser = GCOREParser()
    Util.flipEitherHead(queries.map(parser.apply(_)).toList)

  /** Parse a query and return success or parse error. */
  def parse(src: String): Option[String] =
    parseInternal(src) match
      case Left(err) => Some(err.toString)
      case Right(_)  => None

  /** Infer the type of a query and check with supplied arguments. */
  def inferType(
      src: String,
      iri: String,
      args: Map[String, ArgumentTypeInfo]
  ): Either[String, (String, String, String)] =

    // Obtain initial axioms via IRI (or use empty set).
    val initialAxioms =
      predefinedAxioms.getOrElse(iri, Axioms(Set(), Scopes.default("•")))

    // Parse composition of queries.
    val queries = parseInternal(src)

    // TODO Supply with config form user-end
    val s2s = Shapes2Shapes() // default-configured s2s

    // TODO: Use s2s scopes everywhere (i.e., from user config), including for initialAxioms!

    // TODO Handle arguments

    val resultingAxioms: S2STry[Axioms] = queries.flatMap(qs =>
      // With initial axioms, fold over all queries.
      qs.foldLeft(Right(initialAxioms))((aae: S2STry[Axioms], q: GCORE) =>
        aae.flatMap { ax =>
          val ia = Axioms(ax.toSet, ax.scopes.composeScopes)
          // For each query, we update axioms according to the Algorithm implementation.
          val temp = s2s.constructAxioms(AlgorithmInput.GCOREAxioms(q, ia))
          // println(temp._2) // TODO DEBUGGING
          temp._1
        }
      )
    )

    // The Return statements of the query, if any, in order.
    val returns = queries.toOption.flatMap(qs => qs.last.ret)

    // Collect vocabularies from initial axioms and queries.
    val vocabulary =
      queries.toOption.map { qs =>
        // Collect the scopes of
        //   a) the input shapes, and
        //   b) all queries involved in the composition
        // Drop scopes -- they are re-applied (correctly) later.
        qs.map(q =>
          q.toSCCQ.map(_.vocabulary).getOrElse(Vocabulary.empty)
        ) // Note: On conversion failure, returns an empty vocabulary.
          // This is fine, since conversion failure is detected in other places, anyways.
          .foldLeft(initialAxioms.vocabulary.dropScope(initialAxioms.scopes))(
            (v1, v2) => v1.union(v2).dropScope(initialAxioms.scopes)
          )
      }

    var labels: List[GCORE.Label] = Nil
    var keys: List[(GCORE.Key, String)] = Nil // TODO

    resultingAxioms.map { ax =>
      returns.map { f =>
        vocabulary.map { v =>
          f.kinds.foreach { k =>
            makeTests(k, v.inScope(Scope.Out)(ax.scopes), ax.scopes)
              .foreach { testAxiom =>
                // Note: It would be easier if makeTests would not return Axioms,
                // but tuples / something else where we can get
                // information about whether this is a node/edge etc.
                val result = ax.entails(s2s.getConfig)(testAxiom)

                if result then
                  testAxiom.d.dropScope(ax.scopes) match
                    case NamedConcept(iri) =>
                      val l = GCORE.Label.fromIri(iri, node = true)
                      println("===== DEV:DEBUG " + l)
                      labels = l :: labels
                    case Existential(role, rhs) =>
                      // TODO: Get Key from role.
                      println("===== DEV:DEBUG ----- Why not?")
                      rhs match
                        case NominalConcept(iri) =>
                          iri.value.contains(GCORE.Value.IRI_STRING)
                          println("==== DEV:DEBUG " + role + " " + "string")
                        case Top =>
                          // TODO: Only include T (exists) if there is no more concrete type.
                          // only insert Top when no more specific, overwrite more specifics
                          println("==== DEV:DEBUG " + role + " " + "T")
                        case _ =>
                          println("==== DEV:DEBUG " + role + " " + "...other")
                          ()
                    // TODO
                    // rhs can be T (then 'exists')
                    // or some nominal (then should contain 'int' or 'str')
                    // General way? Concrete value?
                    case _ => println("unknown")
                    // if result then println()
                println(s" $testAxiom : $result")
              }
          }
        }
      }
    }

    // Return either an error or result.
    resultingAxioms match
      case Left(err) => Left(err.toString())
      case Right(ax) => Right(encodeType("node", labels)) // TODO pass keys

    // TODO Get all variables from RETURN
    // TODO Check constraints on these variables over the axioms resultingAxiomsk
    //
    // Find something like the following for the variable n (in RETURN (n))
    //
    // Kind: node, edge, value.
    // "kind" -> "node",
    //
    // For node or edge: properties and types.
    // "props" -> "age:int;name:str",
    //
    // For node or edge: labels
    // "labels" -> "Person"
    //

  /** Encode type information as a String. */
  private def encodeType(
      kind: String,
      labels: List[GCORE.Label]
  ): TypeAnnotation =
    (
      kind,
      labels.map(_.labelname).mkString(";"),
      "age:int;name:str"
    ) // TODO take keys and encode
    // 'int', 'str', or 'exists'

  /** Construct subsumption axioms for testing properties of RESULT clauses. */
  private def makeTests(
      kind: GCORE.Kind,
      voc: Vocabulary,
      scopes: Scopes
  ): Set[Subsumption] =
    // For each 'Kind' construct candidates to check:
    // for node variables, its n <:< concept name (for all concept names in vocabulary)
    //   and property (for all role names)
    // for node ids, check whether n == specific ID (for all nominals in vocabulary)
    // for property, check whether n <:< \exists this property . T (and also what T is, if restricted?)
    //
    kind match
      case GCORE.Kind.Property(v, k) => Set()
      case GCORE.Kind.Var(v)         =>
        // Construct the appropriate concept for the variable 'v'.
        val theVariableConcept = v.toVar.asConcept(scopes)

        // Test subsumption for the variable with all concepts.
        // TODO: Special case meta_node and meta_edge
        val labels = voc.concepts.map { c =>
          Subsumption(theVariableConcept, c)
        }

        // Test subsumption for the variable with all properties.
        val properties = voc.properties.flatMap { p =>
          voc.nominals
            .map { n =>
              Subsumption(
                theVariableConcept,
                Existential(p, NominalConcept(n.dropScope(scopes)))
              )
            }
            // ...and also the base-case with Top.
            .union(Set(Subsumption(theVariableConcept, Existential(p, Top))))
        }

        labels.union(properties)

      case GCORE.Kind.VarRaw(v) => Set()
