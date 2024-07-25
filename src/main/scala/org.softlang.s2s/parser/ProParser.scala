package org.softlang.s2s.parser

import org.softlang.s2s.parser.ProGSBaseVisitor
import org.softlang.s2s.parser.{ProGSLexer, ProGSParser}
import org.softlang.s2s.parser.ProGSParser._

import org.softlang.s2s.query.GCORE

import de.pseifer.shar.core.{Prefix, Iri}
import de.pseifer.shar.parsing.AntlrBasedParser
import de.pseifer.shar.reasoning.{DLReasoner, AxiomSet}

import de.pseifer.shar.dl._
import de.pseifer.shar.error._

import de.pseifer.shar.core.BackendState

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.language.implicitConversions

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.{ParseTreeVisitor, ParseTree};

/** Parse a ProGS shape to a DLExpression. */
class ProParser(state: BackendState)
    extends AntlrBasedParser[
      DLExpression,
      ProGSLexer,
      ProGSParser
    ] {

  // Make internal error message.

  def ierr(location: String): SharError =
    TypeParseError(s"internal parser error ($location)")

  // Set the parsing backend.

  def mkVisitor = DLVisitor()
  def mkLexer(c: CharStream) = ProGSLexer(c)
  def mkParser(t: TokenStream) = ProGSParser(t)
  def doParse(p: ProGSParser) = p.formula()

  def mkSyntaxError(symbol: String, message: String, line: Int): SharError =
    TypeParseError(message)

  // Define the visitor.

  private class DLVisitor
      extends ProGSBaseVisitor[SharTry[DLExpression]]:

    /** Visit one of the binary operators.
      */
    private def visitBinary(
        ctx: IntersectionContext | UnionContext,
        t: (Concept, Concept) => DLExpression
    ): SharTry[DLExpression] =
      for
        left <- visit(ctx.getChild(0))
        right <- visit(ctx.getChild(2))
        c1 <- Concept.orElse(left, ierr("visitBinary"))
        c2 <- Concept.orElse(right, ierr("visitBinary"))
      yield t(c1, c2)

    /** Visit any quantification.
      */
    private def visitQuantification(
        ctx: UniversalContext | ExistentialContext,
        t: (Role, Concept) => DLExpression
    ): SharTry[DLExpression] =
      for
        role <- visit(ctx.getChild(1))
        rhs <- visit(ctx.getChild(3))
        r <- Role.orElse(role, ierr("visitQuantification"))
        c <- Concept.orElse(rhs, ierr("visitQuantification"))
      yield t(r, c)

    /** Visit any edge constraint. */
    private def visitEdgeConstraint(
      ctx: Existential_left_edgeContext | Existential_right_edgeContext
        | Universal_left_edgeContext | Universal_right_edgeContext
        | Left_nodeContext | Right_nodeContext,
      r: Concept => DLExpression
    ): SharTry[DLExpression] =
      for
        rhs <- visit(ctx.getChild(1))
        c <- Concept.orElse(rhs, ierr("visitEdgeConstraint"))
      yield r(c)

    /** Select a single child 'i' and construct type with 't' based on child.
      */
    private def selectChild(
        ctx: Negated_formulaContext | Paren_formulaContext | FormulaContext,
        i: Int,
        t: Concept => DLExpression
    ): SharTry[DLExpression] =
      for
        child <- visit(ctx.getChild(i))
        concept <- Concept.orElse(child, ierr("selectChild"))
      yield t(concept)

    override def visitFormula(ctx: FormulaContext): SharTry[DLExpression] =
      selectChild(ctx, 0, identity)

    override def visitBottom(ctx: BottomContext): SharTry[DLExpression] =
      Right(Bottom)

    override def visitTop(ctx: TopContext): SharTry[DLExpression] =
      Right(Top)

    override def visitConcept(ctx: ConceptContext): SharTry[DLExpression] =
      val rawIri = ctx.getChild(0).getText
      val iri = state.prefixes.expandString(rawIri)
      iri.map(NamedConcept.apply)

    // Other

    override def visitNominal(ctx: NominalContext): SharTry[DLExpression] =
      val rawIri = ctx.getChild(1).getText
      val iri = state.prefixes.expandString(rawIri)
      iri.map(NominalConcept.apply)

    override def visitRole(ctx: RoleContext): SharTry[DLExpression] =
      // -- role --
      if ctx.children.size == 1 then
        val rawIri = ctx.getChild(0).getText
        state.prefixes.expandString(rawIri).map(NamedRole.apply)

      // -- inverse role --
      else
        for
          child <- visit(ctx.getChild(1))
          role <- Role.orElse(child, ierr("visitRole"))
        yield Inverse(role)

    override def visitUnion(ctx: UnionContext): SharTry[DLExpression] =
      visitBinary(ctx, Union.apply)

    override def visitIntersection(
        ctx: IntersectionContext
    ): SharTry[DLExpression] =
      visitBinary(ctx, Intersection.apply)

    override def visitExistential(
        ctx: ExistentialContext
    ): SharTry[DLExpression] =
      visitQuantification(ctx, Existential.apply)

    override def visitUniversal(
        ctx: UniversalContext
    ): SharTry[DLExpression] =
      visitQuantification(ctx, Universal.apply)

    override def visitExistential_left_edge(
      ctx: Existential_left_edgeContext
    ): SharTry[DLExpression] =
      visitEdgeConstraint(ctx, c => Existential(Inverse(GCORE.edgeToNodeRole), c))

    override def visitExistential_right_edge(
      ctx: Existential_right_edgeContext
    ): SharTry[DLExpression] =
      visitEdgeConstraint(ctx, c => Existential(GCORE.nodeToEdgeRole, c))

    override def visitUniversal_left_edge(
      ctx: Universal_left_edgeContext
    ): SharTry[DLExpression] =
      visitEdgeConstraint(ctx, c => Universal(Inverse(GCORE.edgeToNodeRole), c))

    override def visitUniversal_right_edge(
      ctx: Universal_right_edgeContext
    ): SharTry[DLExpression] =
      visitEdgeConstraint(ctx, c => Universal(GCORE.nodeToEdgeRole, c))

    override def visitLeft_node(ctx: Left_nodeContext): SharTry[DLExpression] =
      visitEdgeConstraint(ctx, c => Existential(Inverse(GCORE.nodeToEdgeRole), c))

    override def visitRight_node(ctx: Right_nodeContext): SharTry[DLExpression] =
      visitEdgeConstraint(ctx, c => Existential(GCORE.edgeToNodeRole, c))

    override def visitNegated_formula(
        ctx: Negated_formulaContext
    ): SharTry[DLExpression] =
      selectChild(ctx, 1, Complement.apply)

    override def visitParen_formula(
        ctx: Paren_formulaContext
    ): SharTry[DLExpression] =
      selectChild(ctx, 1, identity)

    override def visitAxiom(ctx: AxiomContext): SharTry[AxiomSet] =
      val axioms = ctx.subsumption.asScala.toList.map { child =>
        for
          c <- visit(child)
          axiom <- Subsumption.orElse(c, ierr("visitAxiom"))
        yield axiom
      }
      SharError
        .getFirst(axioms)
        .map(_.toSet)
        .map(a => AxiomSet(a.map(_.asInstanceOf[Axiom])))

    override def visitSubsumption(ctx: SubsumptionContext): SharTry[Axiom] =
      for
        lhsI <- visit(ctx.getChild(0))
        rhsI <- visit(ctx.getChild(2))
        lhs <- Concept.orElse(lhsI, ierr("visitSubsumption"))
        rhs <- Concept.orElse(rhsI, ierr("visitSubsumption"))
      yield Subsumption(lhs, rhs)
}
