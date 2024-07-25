package org.softlang.s2s.test

import de.pseifer.shar.Shar

import de.pseifer.shar.core._
import de.pseifer.shar.dl._
import de.pseifer.shar.error._
import de.pseifer.shar.reasoning._
import de.pseifer.shar.parsing._

import org.softlang.s2s.parser.ProParser
import org.softlang.s2s.query.GCORE

class ProgsParserTests extends munit.FunSuite:

  private val shar = Shar()
  import shar._

  val parser = ProParser(state)

  def assertConcept(c: String, target: Concept): Unit =
    parser.parse(c) match
      case Left(p)           => assert(false)
      case Right(d: Concept) => assertEquals(d, target)
      case Right(_) => assert(false)

  def shariri(s: String): Iri =
    Iri.fromString("<https://github.com/pseifer/shar/ontology/" ++ s ++ ">").toOption.get

  test("basic concepts") {
    assertConcept("shar:Person", NamedConcept(shariri("Person")))
  }

  test("new edge exist constraints") {
    assertConcept(
      "<-E shar:Person",
      Existential(Inverse(GCORE.edgeToNodeRole), NamedConcept(shariri("Person"))))
    assertConcept(
      "->E shar:Person",
      Existential(GCORE.nodeToEdgeRole, NamedConcept(shariri("Person"))))
    assertConcept(
      "<-∃ shar:Person",
      Existential(Inverse(GCORE.edgeToNodeRole), NamedConcept(shariri("Person"))))
    assertConcept(
      "->∃ shar:Person",
      Existential(GCORE.nodeToEdgeRole, NamedConcept(shariri("Person"))))
  }

  test("new edge forall constraints") {
    assertConcept(
      "<-A shar:Person",
      Universal(Inverse(GCORE.edgeToNodeRole), NamedConcept(shariri("Person"))))
    assertConcept(
      "->A shar:Person",
      Universal(GCORE.nodeToEdgeRole, NamedConcept(shariri("Person"))))
    assertConcept(
      "<-∀ shar:Person",
      Universal(Inverse(GCORE.edgeToNodeRole), NamedConcept(shariri("Person"))))
    assertConcept(
      "->∀ shar:Person",
      Universal(GCORE.nodeToEdgeRole, NamedConcept(shariri("Person"))))
  }

  test("new node constraints") {
    assertConcept(
      "<= shar:Person",
      Existential(Inverse(GCORE.nodeToEdgeRole), NamedConcept(shariri("Person"))))
    assertConcept(
      "=> shar:Person",
      Existential(GCORE.edgeToNodeRole, NamedConcept(shariri("Person"))))
  }
