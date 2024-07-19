package org.softlang.s2s.core

/** Settings for generation of candidates. */
enum ShapeHeuristic(val optimize: Boolean):
    // Generate Simple SHACL shapes (From Shapes To Shapes paper).
    case SimpleShapes(
      // Optimize shapes, removing entailed candidates.
      opt: Boolean = true,
      // Use proxy for family of forall shapes (should always be on)
      proxyFamily: Boolean = true) extends ShapeHeuristic(optimize = opt)
    // Generate additional shapes, useful for ProGS.
    case MediumProGS(
      // Depth of nested existential / universal constraints.
      depth: Int,
      // Breadth of conjunction and disjunction.
      breadth: Int,
      // Sampling factor (0 < factor <= 1.0) for next step, per depth step.
      // That is, for each depths step n, the next step is taken with
      // depthFactor * n probability.
      // depthFactor: Float,
      // Sampling factor (0 < factor <= 1.0) for breadth per depths step.
      // That is, for each depths step n, intersection is sampled with breadthFactor * n.
      // breadthFactor: Float,
      // The seed to use for random sampling.
      // seed: Int
    ) extends ShapeHeuristic(optimize = true)
    // Generate 'all' shapes, as a finite set of relevant shapes.
    // This corresponds to the ALCHOI SHACL set of shapes (From Shapes to Shapes).
    case AllShapes() extends ShapeHeuristic(optimize = true)

object ShapeHeuristic:
  val default = SimpleShapes(
    // CLI override available:
    opt = true,
    // No CLI override available:
    proxyFamily = true,
  )
