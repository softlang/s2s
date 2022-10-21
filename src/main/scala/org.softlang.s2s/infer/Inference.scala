package org.softlang.s2s.infer

import de.pseifer.shar.dl.Axiom

trait Inference:
  def axioms: Set[Axiom]
