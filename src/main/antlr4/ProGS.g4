grammar ProGS;

@header {
package org.softlang.s2s.parser;
}

formula: formula1 EOF;

axiom: (subsumption)+;

subsumption: formula1 SQSUBSETEQ formula1;

formula1: union | formula2;

formula2: intersection | formula3;

formula3:
	paren_formula
	| negated_formula
	| universal
	| existential
    | existential_right_edge
    | existential_left_edge
    | universal_right_edge
    | universal_left_edge
    | left_node
    | right_node
	| top
	| bottom
	| nominal
	| concept;

union: formula3 UNION formula3 | formula3 UNION union;

intersection:
	formula3 INTERSECTION formula3
	| formula3 INTERSECTION intersection;

negated_formula: NOT formula3;

paren_formula: GROUP_LEFT formula1 GROUP_RIGHT;

universal: UNIVERSAL role DOT formula3;

existential: EXISTENTIAL role DOT formula3;

existential_right_edge: EXISTENTIAL_RIGHT formula3;

existential_left_edge: EXISTENTIAL_LEFT formula3;

universal_right_edge: UNIVERSAL_RIGHT formula3;

universal_left_edge: UNIVERSAL_LEFT formula3;

left_node: NODE_LEFT formula3;

right_node: NODE_RIGHT formula3;

top: TOP;

bottom: BOTTOM;

concept: IRI;

nominal: NOMINAL_LEFT IRI NOMINAL_RIGHT;

role: IRI | '-' role;

/* IRI */

IRI: PREFIXED_IRI | FULL_IRI;

FULL_IRI: '<' ~('>')+ '>';

PREFIXED_IRI: PREFIX NAME;

PREFIX: CHARACTER* ':';

NAME: CHARACTER+;

/* TYPES: TODO */

/* TOKEN */

NUMBER: ('0' .. '9')+;

CHARACTER: ('0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_');

WHITESPACE: (' ' | '\t' | '\r' | '\n')+ -> skip;

UNION: '|' | '⊔';

INTERSECTION: '&' | '⊓';

EXISTENTIAL_RIGHT: '->E' | '->∃';

EXISTENTIAL_LEFT: '<-E' | '<-∃';

UNIVERSAL_LEFT: '<-A' | '<-∀';

UNIVERSAL_RIGHT: '->A' | '->∀';

NODE_RIGHT:  '=>' | '⇒';

NODE_LEFT: '<=' | '⇐';

UNIVERSAL: '#A' | '∀';

EXISTENTIAL: '#E' | '∃';

NOT: '!' | '¬';

TOP: '#t' | '⊤';

BOTTOM: '#f' | '⊥';

NOMINAL_LEFT: '{';

NOMINAL_RIGHT: '}';

GROUP_LEFT: '(';

GROUP_RIGHT: ')';

DOT: '.';

SQSUBSETEQ: '⊑' | ':<=';

AXIOMSEP: ';';
