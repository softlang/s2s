package org.softlang.s2s.infer

import org.softlang.s2s.infer

import org.softlang.s2s.query._
import de.pseifer.shar.dl._

trait Assumption(a: AtomicPatterns):
  def axioms: Set[Axiom]
