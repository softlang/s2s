package org.softlang.s2s.generate

import org.softlang.s2s.core.ShapeHeuristic

/** A general, non-query specific problem generator. */
sealed trait ProblemGeneratorConfig:
  /** Generator configuration for shapes. */
  val shapeConfig: ShapeGeneratorConfig

  /** Seed for the random generator; use "" for random initialization. */
  val seed: String = ""

/** A generator configuration for SHACL and ProGS shapes. */
case class ShapeGeneratorConfig(
    /** Minimal number of input shapes. */
    minNumberOfShapes: IntParameter,

    /** Maximal number of input shapes. */
    maxNumberOfShapes: IntParameter,

    /** Number of additional shapes (not only from the vocabulary). Ratio of
      * property-based vs. Concept targets. Set to -1.0 for default.
      */
    propertyConceptTargetRatio: FloatParameter,

    /** Ratio of property (exists, forall) vs. Concept constraints. Set to -1.0
      * for default.
      */
    propertyConceptConstraintRatio: FloatParameter,

    /** Allow universal quantification in constraints. */
    includeForallConstraints: Boolean,

    /** ShapeHeuristic used for sampleing input shapes. */
    sampleHeuristic: ShapeHeuristic
):

  private def fieldNames: List[String] =
    List(
      "MinNumberOfShapes",
      "MaxNumberOfShapes",
      "PropertyConceptTargetRatio",
      "PropertyConceptConstraintRatio",
      "IncludeForallConstraints"
    )

  private def fields: List[String] =
    List(
      minNumberOfShapes,
      maxNumberOfShapes,
      propertyConceptTargetRatio,
      propertyConceptConstraintRatio,
      includeForallConstraints
    ).map(_.toString)

  override def toString: String =
    fieldNames.zip(fields).map((n, f) => s"$n: $f").mkString("\n")

/** A generator configuration for SCCQ queries. */
case class SCCQProblemGeneratorConfig(
    /** Minimal count of atomic patterns in Pattern. */
    minPatternSize: IntParameter,

    /** Maximal count of atomic patterns in Pattern. */
    maxPatternSize: IntParameter,

    /** Minimal count of atomic patterns in Template. */
    minTemplateSize: IntParameter,

    /** Maximal count of atomic patterns in Template. */
    maxTemplateSize: IntParameter,

    /** Probability of generating a fresh variable (0.0 to 1.0). */
    freshVariable: FloatParameter,

    /** Maximum number of variables. */
    variablesCount: IntParameter,

    /** Probability of generating a fresh concept (0.0 to 1.0). */
    freshConcept: FloatParameter,

    /** Total number of concepts allowed. 0 for unlimited. */
    conceptsCount: IntParameter,

    /** Probability of generating a fresh property (0.0 to 1.0). */
    freshProperty: FloatParameter,

    /** Total number of properties allowed. 0 for unlimited. */
    propertiesCount: IntParameter,

    /** Probability of generating a fresh nominal (0.0 to 1.0). */
    freshNominal: FloatParameter,

    /** Total number of nominals allowed. 0 for unlimited. */
    nominalsCount: IntParameter,

    /** Ratio of property patterns to concept patterns (0.0 to 1.0). */
    propertyConceptRatio: FloatParameter,

    /** Ratio of variables to nominals in patterns (0.0 to 1.0). */
    variableToNominalRatio: FloatParameter,

    /** Avoid self-circles by redrawing N times. */
    cyclicRedrawCount: IntParameter,

    /** Shape generation configuration. */
    override val shapeConfig: ShapeGeneratorConfig,

    /** Seed. */
    override val seed: String = ""
) extends ProblemGeneratorConfig:

  private def fieldNames: List[String] =
    List(
      "MinPatternSize",
      "MaxPatternSize",
      "MinTemplateSize",
      "MaxTemplateSize",
      "FreshVariable",
      "VariablesCount",
      "FreshConcept",
      "ConceptsCount",
      "FreshProperty",
      "PropertiesCount",
      "FreshNominal",
      "NominalsCount",
      "PropertyConceptRatio",
      "VariableToNominalRatio",
      "CyclicRedrawCount",
      //
      "ShapeConfig",
      "Seed"
    )

  private def fields: List[String] =
    List(
      minPatternSize,
      maxPatternSize,
      minTemplateSize,
      maxTemplateSize,
      freshVariable,
      variablesCount,
      freshConcept,
      conceptsCount,
      freshProperty,
      propertiesCount,
      freshNominal,
      nominalsCount,
      propertyConceptRatio,
      variableToNominalRatio,
      cyclicRedrawCount,
      //
      shapeConfig,
      seed
    ).map(_.toString)

  override def toString: String =
    fieldNames.zip(fields).map((n, f) => s"$n: $f").mkString("\n")

/** A generator configuration for GCORE queries. */
case class GCOREProblemGeneratorConfig(
    // Fresh and Counts

    /** Probability of generating a fresh value (0.0 to 1.0). */
    freshValue: FloatParameter,

    /** Maximum number of values. */
    valuesCount: IntParameter,

    /** Probability of generating a fresh node variable (0.0 to 1.0). */
    freshVariable: FloatParameter,

    /** Maximum number of node variables. */
    variablesCount: IntParameter,

    /** Probability of generating a fresh key (0.0 to 1.0). */
    freshKey: FloatParameter,

    /** Maximum number of keys. */
    keysCount: IntParameter,

    /** Probability of generating a fresh label (0.0 to 1.0). */
    freshLabel: FloatParameter,

    /** Maximum number of labels. */
    labelsCount: IntParameter,

    // Basic Shape (Patterns)

    /** Target ratio of edge to node patterns. */
    edgeNodeRatio: FloatParameter,

    /** Target minimal number of patterns. */
    minPatterns: IntParameter,

    /** Target maximal number of patterns. */
    maxPatterns: IntParameter,

    /** Probability of node variables in the template to be fresh, unless
      * maximum would be exceeded.
      */
    freshEntities: FloatParameter,

    // Advanced Shape (When, Set, Remove)

    // ...

    /** When producing a loop, redraw edge with this probability. */
    loopRedraw: FloatParameter,

    /** Probability to retain one BGP from the pattern, if there is space. */
    patternRetention: FloatParameter,

    /** Labels attached to each entity when generating when clauses. This is
      * before the target number is selected; thus, determines the likelihood
      * and diversity of labels in WHEN clauses, not their size.
      */
    labelsPerEntity: IntParameter,

    /** Properties attached to each entity when generating when clauses. This is
      * before the target number is selected; thus, determines the likelihood
      * and diversity of labels in WHEN clauses, not their size.
      */
    propsPerEntity: IntParameter,

    /** Set clauses. */
    targetSetClauses: IntParameter,

    /** Remove clauses. */
    targetRemoveClauses: IntParameter,

    /** Remove clauses. */
    targetWhenClauses: IntParameter,

    /** Existence vs value constraints. */
    existToValueConstraints: FloatParameter,

    // General Config

    /** Shape generation configuration. */
    override val shapeConfig: ShapeGeneratorConfig,

    /** Seed. */
    override val seed: String = ""
) extends ProblemGeneratorConfig
