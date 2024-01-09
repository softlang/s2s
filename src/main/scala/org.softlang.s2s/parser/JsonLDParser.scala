package org.softlang.s2s.parser

import java.io.FileReader
import java.io.StringReader

import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.apicatalog.jsonld.document.JsonDocument
import com.apicatalog.jsonld.JsonLd
//import com.apicatalog.jsonld.serialization.RdfToJsonld TODO: Support RDF?

import jakarta.json.JsonValue
import jakarta.json.JsonArray

import de.pseifer.shar.dl._
import org.softlang.s2s.core._
import de.pseifer.shar.core.Iri
import jakarta.json.JsonObject

object JsonLDParser:
  
  // SHACL definitions.
  private object sh:
    val _and = "http://www.w3.org/ns/shacl#and"
    val _class = "http://www.w3.org/ns/shacl#class"
    val _not = "http://www.w3.org/ns/shacl#not"
    val _or = "http://www.w3.org/ns/shacl#or"
    val _property = "http://www.w3.org/ns/shacl#property"
    val _path = "http://www.w3.org/ns/shacl#path"
    val _qualifiedValueShape = "http://www.w3.org/ns/shacl#qualifiedValueShape"
    val _qualifiedMinCount = "http://www.w3.org/ns/shacl#qualifiedMinCount"
    val _targetClass = "http://www.w3.org/ns/shacl#targetClass"
    val _targetObjectsOf = "http://www.w3.org/ns/shacl#targetObjectsOf"
    val _targetSubjectsOf = "http://www.w3.org/ns/shacl#targetSubjectsOf"

  // Wrap nullable values in errors.
  private def wrapn[T](t: T): S2STry[T] =
    if t == null then Left(UnparsableShapeError("malformed JSON value")) else Right(t)

  // Convert a Try to S2STry.
  private def wrap[T](t: Try[T]): S2STry[T] =
    t match
      case Failure(e) => Left(UnparsableShapeError(e.getMessage))
      case Success(v) => Right(v)

  // Parse an Iri, converting to the appropriate error type on failure.
  private def parseIri(s: String): S2STry[Iri] =
    Iri.fromString("<" ++ s ++ ">") match
      case Left(e) => Left(UnparsableShapeError(e.show) )
      case Right(v) => Right(v)

  // TODO
  private def parseT(j: JsonArray, jfn: JsonArray => S2STry[List[String]], fn: Iri => Concept): S2STry[Concept] =
    for 
      s <- jfn(j).map(_.head)
      i <- parseIri(s)
    yield fn(i)

  // Get a field as json array.
  private def getArray(s: String): JsonObject => S2STry[JsonArray] =
    (j: JsonObject) => wrapn(j.getJsonArray(s))

  // Get string from a singleton array, as object.
  private def getString(str: String): JsonArray => S2STry[List[String]] = 
    (j: JsonArray) => wrap(Try { j.iterator().asScala.toList.map(_.asJsonObject().getString(str)) })
  
  // Get string from a singleton array, as object.
  private def getInt(str: String): JsonArray => S2STry[List[Int]] = 
    (j: JsonArray) => wrap(Try { j.iterator().asScala.toList.map(_.asJsonObject().getInt(str)) })

  // Parse the target query as DL Concept.
  private def parseTarget(j: JsonObject): S2STry[Concept] = 
    getArray(sh._targetClass)(j).flatMap(parseT(_, getString("@id"), NamedConcept(_))).orElse(
      getArray(sh._targetObjectsOf)(j).flatMap(parseT(_, getString("@id"), i => Existential(Inverse(NamedRole(i)), Top)))).orElse(
        getArray(sh._targetSubjectsOf)(j).flatMap(parseT(_, getString("@id"), i => Existential(NamedRole(i), Top))))

  private def parsePropertyShape(j: JsonObject): S2STry[Concept] =
    
    // The path, that is mandatory for property shapes.
    val path = getArray(sh._path)(j)
      .flatMap(getString("@id"))
      .flatMap(l => wrap(Try { l.head }))
      .flatMap(parseIri)

    // Potentially an existential, quantified shape 'erhs'.
    val ecount = Right(List(1)) == getArray(sh._qualifiedMinCount)(j)
      .flatMap(getInt("@value"))
    val erhs = getArray(sh._qualifiedValueShape)(j)
      .flatMap(j => wrap(Try { j.iterator().asScala.toList.head.asJsonObject() }))
      .flatMap(parseNodeShape(_))

    // Remaining universal constraints for this path.
    val urhs = Right(
      Concept.intersectionOf(
        List(
          // sh:class
          Util.flipEitherHead(
              getArray(sh._class)(j)
                .flatMap(getString("@id"))
                .toOption.toList.flatten
                .map(parseIri))
            .toOption.toList.flatten
            .map(NamedConcept(_))
          ,
          // sh:not
          getArray(sh._not)(j)
            .flatMap(j => wrap(Try { 
              j.iterator().asScala.toList.head.asJsonObject() 
            }))
            .flatMap(parseNodeShape(_))
            .map(Complement(_)) 
            .toOption.toList
          ,
          // sh:and
          getArray(sh._and)(j)
            .map(_.iterator().asScala.next())
            .flatMap(j => wrap(Try { j.asJsonObject()} ))
            .flatMap(getArray("@list"))
            .map(_.iterator().asScala.toList
              .map(j => parseNodeShape(j.asJsonObject()))
              .map(_.toSeq)
              .flatten)
            .map(Concept.intersectionOf)
            .toSeq
          ,
          // sh:or
          getArray(sh._or)(j)
            .map(_.iterator().asScala.next())
            .flatMap(j => wrap(Try { j.asJsonObject()} ))
            .flatMap(getArray("@list"))
            .map(_.iterator().asScala.toList
              .map(j => parseNodeShape(j.asJsonObject()))
              .map(_.toSeq)
              .flatten)
            .map(Concept.unionOf)
            .toSeq
        ).flatten
      )
    )

    // If qualified, existential shape:
    if ecount then
      for 
        p <- path
        u <- urhs
        e <- erhs 
      yield 
        // Do not include universal part for top.
        if u == Top then
          Existential(NamedRole(p), e)
        // Else, intersection of both.
        else
          Intersection(
            Universal(NamedRole(p), u),
            Existential(NamedRole(p), e))
    // If no qualified shape, include only univeral part.
    else
      for 
        p <- path
        u <- urhs
      yield Universal(NamedRole(p), u)

  // Parse a shape.
  private def parseNodeShape(j: JsonObject): S2STry[Concept] =
    // TODO: Separate property, use function for other.
    Right(
      Concept.intersectionOf(
        List(
          // sh:class
          Util.flipEitherHead(
              getArray(sh._class)(j)
                .flatMap(getString("@id"))
                .toOption.toList.flatten
                .map(parseIri))
            .toOption.toList.flatten
            .map(NamedConcept(_))
          ,
          // sh:property
          getArray(sh._property)(j)
            .map(_.iterator().asScala.toList
              .map(_.asJsonObject())
              .map(parsePropertyShape(_).toSeq.toList)
              .flatten)
            .toSeq
            .flatten
          ,
          // sh:not
          getArray(sh._not)(j)
            .flatMap(j => wrap(Try { 
              j.iterator().asScala.toList.head.asJsonObject() 
            }))
            .flatMap(parseNodeShape(_))
            .map(Complement(_)) 
            .toOption.toList
          ,
          // sh:and
          getArray(sh._and)(j)
            .map(_.iterator().asScala.next())
            .flatMap(j => wrap(Try { j.asJsonObject()} ))
            .flatMap(getArray("@list"))
            .map(_.iterator().asScala.toList
              .map(j => parseNodeShape(j.asJsonObject()))
              .map(_.toSeq)
              .flatten)
            .map(Concept.intersectionOf)
            .toSeq
          ,
          // sh:or
          getArray(sh._or)(j)
            .map(_.iterator().asScala.next())
            .flatMap(j => wrap(Try { j.asJsonObject()} ))
            .flatMap(getArray("@list"))
            .map(_.iterator().asScala.toList
              .map(j => parseNodeShape(j.asJsonObject()))
              .map(_.toSeq)
              .flatten)
            .map(Concept.unionOf)
            .toSeq
        ).flatten
      )
    )

  // Parse the shape constraint as DL Concept.
  private def parseConstraint(j: JsonObject): S2STry[Concept] = parseNodeShape(j)

  // Parse a single shape from a JsonValue string.
  def parse(a: String): S2STry[SHACLShape] = 
    for
      v <- wrap(Try {
            val jdoc = JsonDocument.of(StringReader(a))
            JsonLd.expand(jdoc).get().iterator().next()
          })
      target <- parseTarget(v.asJsonObject())
      constraint <- parseConstraint(v.asJsonObject())
    yield SHACLShape(Subsumption(target, constraint))

  // Load shapes from file, process document and encode as set of JsonValues.
  def fromFile(file: String): Set[String] =
    val jDoc = JsonDocument.of(FileReader(file))
    val arr = JsonLd.expand(jDoc).get()
    arr.iterator.asScala.map(j => j.toString()).toSet

  // Load shapes from string, process document and encode as set of JsonValues.
  def fromString(input: String): Set[String] =
    val jDoc = JsonDocument.of(StringReader(input))
    val arr = JsonLd.expand(jDoc).get()
    arr.iterator.asScala.map(j => j.toString()).toSet
 
