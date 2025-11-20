package org.softlang.s2s.generate

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.{vocabulary => _, _}
import org.softlang.s2s.query._

import scala.util.Random
import scala.annotation.threadUnsafe
import org.antlr.v4.parse.ANTLRParser.throwsSpec_return

// TODO
//
// - [ ] Shape Generation
//   - [ ] Remove 'meta_etn' 'meta_nte' 'meta_node' and 'meta_edge'
//           from general shape generation; needs propert ProGS generator
//           Otherwise, input shapes are too random and do not lead to valid instances in the validator
//             Is this implemented by the ProGS heuristic? It should be!
// - [ ] Query Generation
//   - [ ] TBD

/** Common Generator superclass. */
abstract class ProblemGenerator[Q, S](
    config: ProblemGeneratorConfig,
    attempts: Int = 10
):

  protected var actualMaxShapes = config.shapeConfig.maxNumberOfShapes
  protected var actualMinShapes = config.shapeConfig.minNumberOfShapes

  // Initialize the rnd instance.
  protected val rnd =
    if config.seed == "" then Random() else Random(config.seed.map(_.toInt).sum)

  /** Flip a (weighted) coin. */
  protected def flip(prop: Float = 0.5): Boolean =
    rnd.nextFloat() <= prop

  /** Sample from a range of values. */
  protected def randRange(from: Int, to: Int): Int =
    if from >= to then from
    else rnd.between(from, to + 1)

  /** Make a Iri from String s. */
  protected def mkIri(s: String): Iri = Iri
    .makeFromRawIri(
      "https://github.com/softlang/s2s/" ++ s
    )
    .toOption
    .get

  /** Visualize the generator configuration. */
  def visualize: String = config.toString

  /** Sample a single query of type Q, according to the configuration. */
  def sampleQuery(): Q

  /** Sample a set of shapes of type S, according to the configuration and a
    * given query Q.
    */
  def sampleShapes(q: Q): Set[S]

  /** Draw a query Q and shapes Set[S], retying `attempts` times on count
    * violations.
    */
  protected def doSample(failure: Int): (Q, Set[S]) =
    val q = sampleQuery()
    val s = sampleShapes(q)
    if s.size >= config.shapeConfig.minNumberOfShapes.min || failure >= attempts
    then (q, s)
    else doSample(failure + 1)

  /** Sample a problem instance from this generator. */
  def sample(): (Q, Set[S]) = doSample(0)

/** A generator for G-CORE queries and ProGS shapes, given a
  * ProblemGeneratorConfig.
  */
class ProblemGeneratorPG(config: GCOREProblemGeneratorConfig)(implicit
    scopes: Scopes
) extends ProblemGenerator[GCORE, SHACLShape](config):

  import GCORE._

  /** Generator for values. */
  private val valueGenerator = ThingGenerator[Value](
    config.freshValue.sample(rnd),
    config.valuesCount.sample(rnd),
    id => Value.IntValue(id),
    rnd
  )

  /** Generator for node variables. */
  private val nodeVariableGenerator = ThingGenerator[Variable](
    config.freshVariable.sample(rnd),
    config.variablesCount.sample(rnd),
    id => Variable("n" ++ id.toString),
    rnd
  )

  /** Generator for edge variables. */
  private val edgeVariableGenerator = ThingGenerator[Variable](
    1.0f,
    0,
    id => Variable("e" ++ id.toString),
    rnd
  )

  /** Generator for keys. */
  private val keyGenerator = ThingGenerator[Key](
    config.freshKey.sample(rnd),
    config.keysCount.sample(rnd),
    id => Key("k" ++ id.toString),
    rnd
  )

  /** Generator for edge labels. */
  private val edgeLabelGenerator = ThingGenerator[Label](
    config.freshLabel.sample(rnd),
    config.labelsCount.sample(rnd),
    id => Label("l" ++ id.toString),
    rnd
  )

  /** Generator for node labels. */
  private val nodeLabelGenerator = ThingGenerator[Label](
    config.freshLabel.sample(rnd),
    config.labelsCount.sample(rnd),
    id => Label("L" ++ id.toString),
    rnd
  )

  private def deducedEdge(v1: Variable, v2: Variable): Variable =
    Variable("e" + v1.name + v2.name)

  private def sampleNodeVar(template: Boolean): Variable =
    if template then
      if flip(config.freshEntities.sample(rnd))
      then nodeVariableGenerator.fresh()
      else nodeVariableGenerator.select()
    else nodeVariableGenerator.sample()

  /** Generate a node pattern. */
  private def generateNodePattern(template: Boolean): BasicGraphPattern =
    val v = sampleNodeVar(template)
    BasicGraphPattern.NodePattern(v)

  /** Generate an edge pattern. */
  private def generateEdgePattern(template: Boolean): BasicGraphPattern =
    val v1 = sampleNodeVar(template)
    val v2 = sampleNodeVar(template)

    if (v1 == v2 && flip(config.loopRedraw.sample(rnd)))
      generateEdgePattern(template)
    else
      val e =
        if template then
          if flip(config.freshEntities.sample(rnd))
          then edgeVariableGenerator.fresh()
          else deducedEdge(v1, v2)
        else deducedEdge(v1, v2)
      BasicGraphPattern.EdgePattern(v1, e, v2)

  /** Generate basic graph pattern. */
  private def generateBGP(template: Boolean): BasicGraphPattern =
    if flip(config.edgeNodeRatio.sample(rnd)) then generateEdgePattern(template)
    else generateNodePattern(template)

  /** Generate a random full graph pattern. */
  private def generateFGP(template: Boolean): FullGraphPattern =
    Set
      .fill(config.maxPatterns.max)(generateBGP(template))
      .take(
        randRange(
          config.minPatterns.sample(rnd),
          config.maxPatterns.sample(rnd)
        )
      )

  def generateWhen(pVars: Set[Variable]): Set[WhenClause] =

    val nl = nodeVariables(pVars).flatMap: v =>
      val r = config.labelsPerEntity.sample(rnd)
      Set.fill(r)(WhenClause.HasLabel(v, nodeLabelGenerator.sample()))

    val nk = nodeVariables(pVars).flatMap: v =>
      val r = config.propsPerEntity.sample(rnd)
      Set.fill(r)(
        if flip(config.existToValueConstraints.sample(rnd)) then
          WhenClause.HasKey(v, keyGenerator.sample())
        else
          WhenClause
            .HasKeyValue(v, keyGenerator.sample(), valueGenerator.sample())
      )

    val el = edgeVariables(pVars).flatMap: v =>
      val r = config.labelsPerEntity.sample(rnd)
      Set.fill(r)(WhenClause.HasLabel(v, edgeLabelGenerator.sample()))

    val ek = edgeVariables(pVars).flatMap: v =>
      val r = config.propsPerEntity.sample(rnd)
      Set.fill(r)(
        if flip(config.existToValueConstraints.sample(rnd)) then
          WhenClause.HasKey(v, keyGenerator.sample())
        else
          WhenClause
            .HasKeyValue(v, keyGenerator.sample(), valueGenerator.sample())
      )

    rnd
      .shuffle(el.union(nl).union(ek).union(nk).toList)
      .take(config.targetWhenClauses.sample(rnd))
      .toSet

  def generateSet(pVars: Set[Variable], tVars: Set[Variable]): Set[SetClause] =
    Set.fill(config.targetSetClauses.sample(rnd)) {
      if edgeVariables(pVars.union(tVars)).isEmpty || flip(0.5) then
        val vs = nodeVariables(pVars.union(tVars))
        val v = Random.shuffle(vs.toList).head
        if flip(0.8) then SetClause.SetLabel(v, nodeLabelGenerator.sample())
        else
          SetClause.SetKeyValue(
            v,
            keyGenerator.sample(),
            valueGenerator.sample()
          )
      else
        val vs = edgeVariables(pVars.union(tVars))
        val v = Random.shuffle(vs.toList).head
        if flip(0.8) then SetClause.SetLabel(v, edgeLabelGenerator.sample())
        else
          SetClause.SetKeyValue(
            v,
            keyGenerator.sample(),
            valueGenerator.sample()
          )
    }

  def generateRemove(pVars: Set[Variable]): Set[RemoveClause] =
    Set.fill(config.targetRemoveClauses.sample(rnd)) {
      if edgeVariables(pVars).isEmpty || flip(0.5) then
        val vs = nodeVariables(pVars)
        val v = Random.shuffle(vs.toList).head
        if flip(0.8) then
          RemoveClause.RemoveLabel(v, nodeLabelGenerator.sample())
        else RemoveClause.RemoveKey(v, keyGenerator.sample())
      else
        val vs = edgeVariables(pVars)
        val v = Random.shuffle(vs.toList).head
        if flip(0.8) then
          RemoveClause.RemoveLabel(v, edgeLabelGenerator.sample())
        else RemoveClause.RemoveKey(v, keyGenerator.sample())
    }

  private def nodeVariables(vars: Set[Variable]): Set[Variable] =
    vars.filter(v => v.name.startsWith("n"))

  private def edgeVariables(vars: Set[Variable]): Set[Variable] =
    vars.filter(v => v.name.startsWith("e"))

  /** Sample a query instance, only. */
  def sampleQuery(): GCORE =

    // Reset the generators.
    nodeVariableGenerator.reset()
    edgeVariableGenerator.reset()
    nodeLabelGenerator.reset()
    edgeLabelGenerator.reset()
    keyGenerator.reset()
    valueGenerator.reset()

    val pattern = generateFGP(template = false)
    val templatePreExt = generateFGP(template = true)

    val template =
      if templatePreExt.size < config.maxPatterns.sample(rnd) && flip(
          config.patternRetention.sample(rnd)
        )
      then
        templatePreExt.union(
          Random
            .shuffle(pattern.toList)
            .take(1)
            .toSet
        )
      else templatePreExt

    val patternVars = pattern.flatMap(_.variables)
    val templateVars = template.flatMap(_.variables)

    val w = generateWhen(patternVars)
    val s = generateSet(patternVars, templateVars)
    val r = generateRemove(patternVars)

    val gq = GCORE(Construct(template, s, r), Match(pattern, w))

    // Just try again on invalid queries; then, validity
    // is pushed into GCORE.validate() and might be extended
    // there, where required.
    if (!gq.validate()) then sampleQuery() else gq

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

  /** Filter shapes by validity. */
  private def valid(shape: SHACLShape): Boolean =
    val str = shape.toString()
    List(
      // Valid shapes must not contain type constraints, since the validation
      // procedure currently does not handle them.
      !str.contains("https://github.com/softlang/s2s/int-")
      // TODO
    ).reduce(_ && _)

  /** Sample a set of SHACLShapes */
  def sampleShapes(qi: GCORE): Set[SHACLShape] =

    val q = qi.toSCCQ match
      case None =>
        throw new RuntimeException("Non-convertable GCORE query in Generator.")
      case Some(qq) => qq

    // Maybe 50:50 simple subset and full?
    //
    // val simple = ShapeGenerator(
    //   q.pattern.vocabulary.union(q.template.vocabulary),
    //   ShapeHeuristic.NovaProGS(1, 1, opt = false)
    // ).generate.filter(valid)

    val full = ShapeGenerator(
      q.pattern.vocabulary.union(q.template.vocabulary),
      config.shapeConfig.sampleHeuristic
    ).generate.filter(valid)

    // Randomly select required subset from filtered shapes.
    rnd
      .shuffle(full.toList)
      .take(
        randRange(
          config.shapeConfig.minNumberOfShapes.sample(rnd),
          config.shapeConfig.maxNumberOfShapes.sample(rnd)
        )
      )
      .toSet

/** A generator for SPARQL queries and SHACL shapes, given a
  * ProblemGeneratorConfig.
  */
class ProblemGeneratorRDF(config: SCCQProblemGeneratorConfig)(implicit
    scopes: Scopes
) extends ProblemGenerator[SCCQ, SHACLShape](config):

  import AtomicPattern._

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

  /** Sample a set of SHACLShapes */
  def sampleShapes(q: SCCQ): Set[SHACLShape] =

    // The complete set of possible shapes.
    val initial = ShapeGenerator(
      q.pattern.vocabulary.union(q.template.vocabulary),
      config.shapeConfig.sampleHeuristic
    ).generate.map(_.toSimple).filter(_.nonEmpty).map(_.get)

    // Remove forall shapes, if they are not allowed.
    val allowed =
      if config.shapeConfig.includeForallConstraints then initial
      else initial.filter(!_.isForallShape)

    // Filter according to ratios for target/constraint.
    val filtered1 =
      ratioalize(
        allowed,
        !_.isConceptShape,
        config.shapeConfig.propertyConceptConstraintRatio.sample(rnd)
      )

    val filtered = ratioalize(
      filtered1,
      _.hasExistentialTarget,
      config.shapeConfig.propertyConceptTargetRatio.sample(rnd)
    )

    // Randomly select required subset from filtered shapes.
    rnd
      .shuffle(filtered.toList)
      .take(
        randRange(
          config.shapeConfig.minNumberOfShapes.sample(rnd),
          config.shapeConfig.maxNumberOfShapes.sample(rnd)
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
