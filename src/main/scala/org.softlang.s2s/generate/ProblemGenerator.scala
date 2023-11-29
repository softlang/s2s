package org.softlang.s2s.generate

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.{vocabulary => _, _}
import org.softlang.s2s.query._

import scala.util.Random

/** A generator for queries, given a ProblemGeneratorConfig. */
class ProblemGenerator(config: ProblemGeneratorConfig)(implicit scopes: Scopes):

  import AtomicPattern._

  private var actualMaxShapes = config.maxNumberOfShapes
  private var actualMinShapes = config.minNumberOfShapes

  private val loader = FileLoader()

  // Initialize the rnd instance.
  private val rnd =
    if config.seed == "" then Random() else Random(config.seed.map(_.toInt).sum)

  /** Flip a (weighted) coin. */
  private def flip(prop: Float = 0.5): Boolean =
    rnd.nextFloat() <= prop

  /** Sample from a range of values. */
  private def randRange(from: Int, to: Int): Int =
    if from >= to then from
    else rnd.between(from, to + 1)

  /** Make a Iri from String s. */
  private def mkIri(s: String): Iri = Iri
    .makeFromRawIri(
      "https://github.com/softlang/s2s/" ++ s
    )
    .toOption
    .get

  /** Generator for nominals. */
  private val nominalGenerator = ThingGenerator[Iri](
    config.freshNominal.sample(rnd),
    config.nominalsCount.sample(rnd),
    id => mkIri("a" ++ id.toString),
    rnd
  )

  /** Generator for variables. */
  private val variableGenerator = ThingGenerator[Var](
    config.freshVariable.sample(rnd),
    config.variablesCount.sample(rnd),
    id => Var("v" ++ id.toString),
    rnd
  )

  /** Generator for properties. */
  private val roleGenerator = ThingGenerator[NamedRole](
    config.freshConcept.sample(rnd),
    config.conceptsCount.sample(rnd),
    id => NamedRole(mkIri("p" ++ id.toString)),
    rnd
  )

  /** Generator for concepts. */
  private val conceptGenerator = ThingGenerator[NamedConcept](
    config.freshConcept.sample(rnd),
    config.conceptsCount.sample(rnd),
    id => NamedConcept(mkIri("C" ++ id.toString)),
    rnd
  )

  /** Generator for concept atomic patterns. */
  private def generateCP: AtomicPattern =
    if flip(config.variableToNominalRatio.sample(rnd)) then
      VAC(variableGenerator.sample(), conceptGenerator.sample().c)
    else LAC(nominalGenerator.sample(), conceptGenerator.sample().c)

  /** Generator for property atomic patterns. */
  private def generatePP: AtomicPattern =
    if flip(config.variableToNominalRatio.sample(rnd)) then
      // sample the first variable.
      val v1 = variableGenerator.sample()
      // (Re)sample the second if needed.
      var v2 = variableGenerator.sample()
      var counter = 0
      var max = config.cyclicRedrawCount.sample(rnd)
      while (counter < max && v1 == v2) {
        v2 = variableGenerator.sample()
        counter += 1
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
    if flip(config.propertyConceptRatio.sample(rnd)) then generatePP
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
  def sample(): (SCCQ, Set[SHACLShape]) =
    if config.inputFile.isDefined then fileSample()
    else doSample(0)

  private def fileSample(): (SCCQ, Set[SHACLShape]) =
    val q =
      if loader.isLoaded then loader.getSample()
      else
        loader.load(config.inputFile.get)
        loader.getSample()
    val s = sampleShapes(q)
    (q, s)

  private def doSample(failure: Int): (SCCQ, Set[SHACLShape]) =
    val q = sampleQuery()
    val s = sampleShapes(q)

    // If minimum number of shapes is violated (e.g., due to unlikely ratios)
    // retry. In total, we retry 10 times. Then, we accept this violation.

    if s.size >= config.minNumberOfShapes.min || failure >= 10 then (q, s)
    else doSample(failure + 1)

  /** Sample a set of SHACLShapes */
  def sampleShapes(q: SCCQ): Set[SHACLShape] =

    // The complete set of possible shapes.
    val initial = ShapeGenerator(
      q.pattern.vocabulary.union(q.template.vocabulary),
      ShapeHeuristic.default
    ).generate.map(_.toSimple).filter(_.nonEmpty).map(_.get)

    // Remove forall shapes, if they are not allowed.
    val allowed =
      if config.includeForallConstraints then initial
      else initial.filter(!_.isForallShape)

    // Filter according to ratios for target/constraint.
    val filtered1 =
      ratioalize(
        allowed,
        !_.isConceptShape,
        config.propertyConceptConstraintRatio.sample(rnd)
      )

    val filtered = ratioalize(
      filtered1,
      _.hasExistentialTarget,
      config.propertyConceptTargetRatio.sample(rnd)
    )

    // Randomly select required subset from filtered shapes.
    rnd
      .shuffle(filtered.toList)
      .take(
        randRange(
          config.minNumberOfShapes.sample(rnd),
          config.maxNumberOfShapes.sample(rnd)
        )
      )
      .toSet

  /** Sample a query instance, only. */
  def sampleQuery(): SCCQ =

    // Reset the generators.
    variableGenerator.reset()
    conceptGenerator.reset()
    roleGenerator.reset()
    nominalGenerator.reset()

    val pattern = Set
      .fill(config.maxPatternSize.sample(rnd))(generate)
      .take(
        randRange(
          config.minPatternSize.sample(rnd),
          config.maxPatternSize.sample(rnd)
        )
      )

    // Do not generate fresh variables for template.
    variableGenerator.lock()
    variableGenerator.setThings(pattern.flatMap(_.variables))

    val template = Set
      .fill(config.maxTemplateSize.sample(rnd))(generate)
      .take(
        randRange(
          config.minTemplateSize.sample(rnd),
          config.maxTemplateSize.sample(rnd)
        )
      )

    SCCQ(template.toList, pattern.toList)
