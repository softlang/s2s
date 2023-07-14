package org.softlang.s2s.core

/** Settings for generation of candidates. */
case class ShapeHeuristic(
    // Generate only simple SHACL shapes. Overrides all other settings.
    simpleShapes: Boolean,

    // Use a proxy for the family of form C ⊑ ∀p.P, instead of ignoring them.
    proxyFamily: Boolean,

    // Optimize candidate generation by omitting entailed shapes.
    // Can be further refined for non-simple SHACL shapes.
    optimize: Boolean
)

object ShapeHeuristic:
  val default = ShapeHeuristic(
    // CLI override available:
    optimize = true,
    // No CLI override available:
    proxyFamily = true,
    simpleShapes = false
  )
