package org.softlang.s2s.generate

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core._
import org.softlang.s2s.query._

import scala.util.Random

/** A generator for queries, given a ProblemGeneratorConfig. */
class ProblemGenerator(config: ProblemGeneratorConfig)(implicit scopes: Scopes):

  import AtomicPattern._

  private var actualMaxShapes = config.maxNumberOfShapes
  private var actualMinShapes = config.minNumberOfShapes

  /** Flip a (weighted) coin. */
  private def flip(prop: Float = 0.5): Boolean =
    Random.nextFloat() <= prop

  /** Sample from a range of values. */
  private def randRange(from: Int, to: Int): Int =
    if from >= to then Random.between(to - 1, to)
    else Random.between(from, to)

  /** Make a Iri from String s. */
  private def mkIri(s: String): Iri = Iri
    .makeFromRawIri(
      "https://github.com/softlang/s2s/" ++ s
    )
    .toOption
    .get

  /** Generator for nominals. */
  private val nominalGenerator = ThingGenerator[Iri](
    config.freshNominal.sample,
    config.nominalsCount.sample,
    id => mkIri("a" ++ id.toString)
  )

  /** Generator for variables. */
  private val variableGenerator = ThingGenerator[Var](
    config.freshVariable.sample,
    config.variablesCount.sample,
    id => Var("v" ++ id.toString)
  )

  /** Generator for properties. */
  private val roleGenerator = ThingGenerator[NamedRole](
    config.freshConcept.sample,
    config.conceptsCount.sample,
    id => NamedRole(mkIri("p" ++ id.toString))
  )

  /** Generator for concepts. */
  private val conceptGenerator = ThingGenerator[NamedConcept](
    config.freshConcept.sample,
    config.conceptsCount.sample,
    id => NamedConcept(mkIri("C" ++ id.toString))
  )

  /** Generator for concept atomic patterns. */
  private def generateCP: AtomicPattern =
    if flip(config.variableToNominalRatio.sample) then
      VAC(variableGenerator.sample(), conceptGenerator.sample().c)
    else LAC(nominalGenerator.sample(), conceptGenerator.sample().c)

  /** Generator for property atomic patterns. */
  private def generatePP: AtomicPattern =
    if flip(config.variableToNominalRatio.sample) then
      // sample the first variable.
      val v1 = variableGenerator.sample()
      // (Re)sample the second if needed.
      var v2 = variableGenerator.sample()
      var counter = 0
      var max = config.cyclicRedrawCount.sample
      while(counter < max && v1 == v2) {
        v2 = variableGenerator.sample()
      }

      VPV(v1, roleGenerator.sample().r, v2)
    else {
      if flip(0.333) then
        VPL(
          variableGenerator.sample(),
          roleGenerator.sample().r,
          nominalGenerator.sample()
        )
      else if flip() then
        LPL(
          nominalGenerator.sample(),
          roleGenerator.sample().r,
          nominalGenerator.sample()
        )
      else
        LPV(
          nominalGenerator.sample(),
          roleGenerator.sample().r,
          variableGenerator.sample()
        )
    }

  /** Generate atomic pattern. */
  private def generate: AtomicPattern =
    if flip(config.propertyConceptRatio.sample) then generatePP
    else generateCP

  /** Adapt shape candidates to set ratio for some marker. */
  private def ratioalize(
      shapes: Set[SimpleSHACLShape],
      marker: SimpleSHACLShape => Boolean,
      ratio: Float
  ): Set[SimpleSHACLShape] =
    // For negative ratio, disable filtering.
    if ratio < 0.0f then shapes
    else
      val lr = shapes.partition(marker)
      val marked = lr._1
      val unmarked = lr._2

      if unmarked.isEmpty && ratio == 1.0f then marked
      else if marked.isEmpty && ratio == 0.0f then unmarked
      else if marked.isEmpty || unmarked.isEmpty then
        actualMaxShapes.mult(ratio)
        actualMinShapes.mult(ratio)
        shapes
      else
        val actual =
          if unmarked.isEmpty then 1.0f
          else marked.size.toFloat / (shapes.size.toFloat)

        val result =
          if marked.size == unmarked.size then {
            if ratio == 0.5 then shapes
            else if ratio >= 0.5 then reduceRight(marked, unmarked, ratio)
            else reduceRight(unmarked, marked, 1.0f - ratio)
          } else if marked.size > unmarked.size then {
            if actual > ratio then reduceRight(unmarked, marked, 1.0f - ratio)
            else reduceRight(marked, unmarked, ratio)
          } else {
            if actual > ratio then reduceRight(unmarked, marked, 1.0f - ratio)
            else reduceRight(marked, unmarked, ratio)
          }

        result

  private def reduceRight(
      left: Set[SimpleSHACLShape],
      right: Set[SimpleSHACLShape],
      targetRatio: Float
  ): Set[SimpleSHACLShape] =
    if right.isEmpty then left
    else if left.size.toFloat / (left.union(right).size.toFloat) >= targetRatio
    then left.union(right)
    else reduceRight(left, right.tail, targetRatio)

  /** Visualize the generator configuration. */
  def visualize: String = config.toString

  /** Sample a problem instance from this generator. */
  def sample(): (SCCQ, Set[SimpleSHACLShape]) =
    val q = sampleQuery()
    (q, sampleShapes(q))

  /** Sample a set of SimpleSHACLShapes */
  def sampleShapes(q: SCCQ): Set[SimpleSHACLShape] =

    // The complete set of posible shapes.
    val initial = ShapeGenerator(q.pattern.vocabulary, true).generate

    // Remove forall shapes, if they are not allowed.
    val allowed =
      if config.includeForallConstraints then initial
      else initial.filter(!_.isForallShape)

    // Filter according to ratios for target/constraint.
    val filtered1 =
      ratioalize(
        allowed,
        !_.isConceptShape,
        config.propertyConceptConstraintRatio.sample
      )

    val filtered = ratioalize(
      filtered1,
      _.hasExistentialTarget,
      config.propertyConceptTargetRatio.sample
    )

    // Randomly select requierd subset from filtered shapes.
    Random
      .shuffle(filtered.toList)
      .take(
        randRange(
          config.minNumberOfShapes.sample,
          config.maxNumberOfShapes.sample
        )
      ).toSet

  /** Sample a query instance, only. */
  def sampleQuery(): SCCQ =

    // Reset the generators.
    variableGenerator.reset()
    conceptGenerator.reset()
    roleGenerator.reset()
    nominalGenerator.reset()

    val pattern = Set
      .fill(config.maxPatternSize.sample)(generate)
      .take(
        randRange(config.minPatternSize.sample, config.maxPatternSize.sample)
      )

    // Do not generate fresh variables for template.
    variableGenerator.lock()
    variableGenerator.setThings(pattern.flatMap(_.variables))

    val template = Set
      .fill(config.maxTemplateSize.sample)(generate)
      .take(
        randRange(config.minTemplateSize.sample, config.maxTemplateSize.sample)
      )

    SCCQ(template.toList, pattern.toList)
