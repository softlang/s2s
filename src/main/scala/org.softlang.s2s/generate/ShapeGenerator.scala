package org.softlang.s2s.generate

import org.softlang.s2s.core.Vocabulary
import org.softlang.s2s.core.SimpleSHACLShape

import scala.util.Random

class ShapeGenerator(voc: Vocabulary, number: Int, optimize: Boolean = true):

    /** Generate all possible shapes over vocabulary. */
    private val axioms = CandidateGenerator(voc, optimize).axioms

    /** Drawn 'n' simple SHACL shapes. */
    def draw: Set[SimpleSHACLShape] =
        Random.shuffle(axioms.toList).take(number).toSet
