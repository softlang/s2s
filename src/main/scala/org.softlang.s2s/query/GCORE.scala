package org.softlang.s2s.query

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Showable
import de.pseifer.shar.core.Iri

import org.softlang.s2s.core.S2STry
import org.softlang.s2s.core.Util
import org.softlang.s2s.core.UnsupportedQueryError
import org.softlang.s2s.core.Var
import org.softlang.s2s.query.AtomicPattern

/** Representation of a G-CORE (query) as match and construct clause.
  */
class GCORE(
    val template: GCORE.Construct,
    val pattern: GCORE.Match
) extends Showable:

  import GCORE._

  def show(implicit state: BackendState): String =
    "CONSTRUCT " ++ template.show ++ "\nMATCH " ++ pattern.show

  private def generateNodes(fgp: FullGraphPattern, vars: Set[Var], lok: Map[Variable, List[WhenClause]]): Option[Set[AtomicPattern]] = 
    // Iterate all NodePattern.
    Util.sequence(fgp.toList.map { 
      case BasicGraphPattern.NodePattern(x) => 
        // Find all labels for x. 
        val labels = lok.getOrElse(x, Nil).flatMap { x => x match
          case WhenClause.HasLabel(_, l) => Some(l)
          case _ => None
        }
        // TODO: Incorporate key-value pairs.
        // Must be at least one for a valid SCCQ, or 'x' must be in vars.
        if labels.isEmpty && !vars.contains(x.toVar) then None
        else 
          val vx = x.toVar
          // Generate VAC pattern for all labels.
          Some(labels.map { l =>
            AtomicPattern.VAC(vx, l.toIri)
          })
      case BasicGraphPattern.EdgePattern(_, _, _) => Some(Nil)
    }).map(_.flatten.toSet)

  private def generateEdges(fgp: FullGraphPattern, lok: Map[Variable, List[WhenClause]]): Option[Set[AtomicPattern]] =
    // Iterate all EdgePattern.
    Util.sequence(fgp.toList.map { 
      case BasicGraphPattern.EdgePattern(x, p, y) => 
        // Find all labels for p. 
        val labels = lok.getOrElse(p, Nil).flatMap { p => p match
          case WhenClause.HasLabel(_, l) => Some(l)
          case _ => None
        }
        // TODO: Incorporate key-value pairs.
        // Must be at least one for a valid SCCQ.
        if labels.isEmpty then None
        else 
          val vx = x.toVar
          val vy = y.toVar
          // Generate VPV pattern for all labels.
          Some(labels.map { l =>
            AtomicPattern.VPV(vx, l.toIri, vy)
          })
      case BasicGraphPattern.NodePattern(_) => Some(Nil)
    }).map(_.flatten.toSet)

  private def generateAtomic(fgp: FullGraphPattern, clauses: Set[WhenClause]): Option[AtomicPatterns] = 
    // A lookup map from variables to related constraints (WhenClause).
    val lok = clauses.map(_.toVarTuple).groupMapReduce(_._1)(x => List(x._2))(_ ++ _)
    for 
      // Generate the edges.
      edges <- generateEdges(fgp, lok)
      // Generate additional patterns from nodes.
      nodes <- generateNodes(fgp, edges.toList.variables, lok)
    yield edges.union(nodes).toList

  def toSCCQ: Option[SCCQ] =
    for h <- generateAtomic(
          template.fullGraphPattern, 
          pattern.when
            .union(template.set.map(_.asWhen))
            .diff(template.remove.map(_.asWhen)))
        p <- generateAtomic(
          pattern.fullGraphPattern, 
          pattern.when)
    yield SCCQ(
      template = h,
      pattern = p
    )

//class GCoreToSparql(prefix: String):
//
//  import Syntax._
//
//  def convert(gcore: Query): String = 
//    convert(gcore._1) ++ convert(gcore._2)
//
//  private def convert(c: ConstructClause): String = ""
//
//  private def convert(c: MatchClause): String = c match
//    case MatchClause.Match(p) => s"WHERE { ${convert(p)} }"
//    case MatchClause.MatchWhere(p, w) => s"WHERE { ${convert(p)} . ${convert(w)} }"
//
//  private def convert(: FullGraphPattern): String = 
//    p.map(convert).filter(_ != "").mkString(" . ")
//
//  private def convert(p: BasicGraphPattern): String = p match
//    case BasicGraphPattern.NodePattern(x) => ""
//    case BasicGraphPattern.EdgePattern(x, z, y) => "tbd"
//
//  private def convert(w: WhenClause): String = ""
//
//  private def convert(v: Variable): String = s"?${v.name}"

object GCORE:

  case class Variable(name: String) extends Showable:
    def show(implicit state: BackendState): String = name

    def toVar: Var = Var(name)

  case class Key(keyname: String) extends Showable:
    def show(implicit state: BackendState): String = "." ++ keyname

  case class Label(labelname: String) extends Showable:
    def show(implicit state: BackendState): String = ":" ++ labelname

    def toIri: Iri = 
      // Hack.
      Iri.fromString("<https://github.com/softlang/s2s/" ++ labelname ++">").toOption.get
  
  enum Value extends Showable:
    case IntValue(int: Int)
    case StringValue(string: String)
    case BooleanValue(bool: Boolean)

    def show(implicit state: BackendState): String = this match
      case IntValue(i) => i.toString
      case StringValue(s) => s"\"$s\""
      case BooleanValue(b) => b.toString

  //type Query = BasicGraphQuery

  // Query (advanced)
  // type Query = FullGraphQuery
  //
  // enum FullGraphQuery:
  //   case Basic(basic: BasicGraphQuery)
  //   case Op(op: SetOp, left: FullGraphQuery, right: FullGraphQuery)

  // enum SetOp:
  //   case Union
  //   case Intersect
  //   case Minus

  type BasicGraphQuery = (Construct, Match)

  // CONSTRUCT (simplified)

  case class Construct(fullGraphPattern: FullGraphPattern, set: Set[SetClause], remove: Set[RemoveClause]) extends Showable:
    def show(implicit state: BackendState): String = this match
      case Construct(fgp, s, r) => 
        val bs = fgp.map(_.show).mkString(", ")
        val ss = if s.nonEmpty then "\nSET" ++ s.map(_.show).mkString(" AND ") else ""
        val rs = if s.nonEmpty then "\nREMOVE" ++ r.map(_.show).mkString(" AND ") else ""
        bs ++ ss ++ rs

    //def toSCCQ: S2STry[AtomicPatterns] =
    //  this match
    //    case Construct(fgp) => fgpToSCCQ(fgp)
    //    case ConstructS(fgp, sc) => fa
      

    //case ConstructWhen(fullGraphPattern: FullGraphPattern, booleanCondition: WhenClause)

  // CONSTRUCT (advanced)
  //type ConstructClause = Set[BasicConstruct]

  //enum BasicConstruct:
  //  case Construct(constructList: ConstructList)
  //  case ConstructWhen(constructList: ConstructList, eta: WhenClause)

  //type ConstructList = Set[ObjectConstruct]

  //enum ObjectConstruct:
  //  case NodeConstruct(x: Variable)
  //  case EdgeConstruct(x: Variable,  z: Variable, y: Variable)

  enum RemoveClause extends Showable:
    case RemoveKey(x: Variable, k: Key)
    case RemoveLabel(x: Variable, l: Label)

    def show(implicit state: BackendState): String = this match
      case RemoveKey(x, k) => x.show ++ k.show
      case RemoveLabel(x, l) => x.show ++ l.show

    def asWhen: WhenClause = this match
      case RemoveKey(x, k) => WhenClause.HasKey(x, k)
      case RemoveLabel(x, l) => WhenClause.HasLabel(x, l)

  enum SetClause extends Showable:
    case SetKeyValue(x: Variable, k: Key, v: Value)
    case SetLabel(x: Variable, l: Label)

    def show(implicit state: BackendState): String = this match
      case SetKeyValue(x, k, v) => s"${x.show}${k.show} = ${v.show}"
      case SetLabel(x, l) => x.show ++ l.show

    def asWhen: WhenClause = this match
      case SetKeyValue(x, k, v) => WhenClause.HasKeyValue(x, k, v)
      case SetLabel(x, l) => WhenClause.HasLabel(x, l)

  enum WhenClause extends Showable:
    case HasKey(x: Variable, k: Key)
    case HasKeyValue(x: Variable, k: Key, v: Value)
    case HasLabel(x: Variable, l: Label)

    def show(implicit state: BackendState): String = this match
      case HasKey(x, k) => x.show ++ k.show
      case HasKeyValue(x, k, v) => s"${x.show}${k.show} = ${v.show}"
      case HasLabel(x, l) => x.show ++ l.show

    /** Get the variable of this WhenClause. */
    def getVar: Variable = this match
      case HasKey(x, k) => x
      case HasKeyValue(x, k, v) => x
      case HasLabel(x, l) => x

    /** Get a mapping from getVar to this WhenClause. */
    def toVarTuple: (Variable, WhenClause) = (this.getVar -> this)

    // TODO ...

  // MATCH
  case class Match(fullGraphPattern: FullGraphPattern, when: Set[WhenClause]) extends Showable:

    def show(implicit state: BackendState): String = this match
        case Match(fgp, c) => 
          val bs = fgp.map(_.show).mkString(", ")
          val ws = if c.nonEmpty then "\nWHERE " ++ c.map(_.show).mkString(" AND ") else ""
          bs ++ ws

    //def toSCCQ: S2STry[AtomicPatterns] =
    //  this match
    //    case Match(fgp) => fgpToSCCQ(fgp)
    //    case MatchWhere(fgp, c) => fgpToSCCQ(fgp, c)

  type FullGraphPattern = Set[BasicGraphPattern]
    
  enum BasicGraphPattern extends Showable:
    case NodePattern(x: Variable)
    case EdgePattern(x: Variable, z: Variable, y: Variable)

    def show(implicit state: BackendState): String = this match
        case NodePattern(v) => s"(${v.show})"
        case EdgePattern(x, z, y) => s"(${x.show})-[${z.show}]->(${y.show})"

    //def toSCCQ: S2STry[AtomicPatterns] =
    //  this match
    //    case NodePattern(v) => Left(UnsupportedQueryError(this, "can not be converted to SCCQ"))
    //    case EdgePattern(x, z, y) => Left(UnsupportedQueryError(this, "can not be converted to SCCQ"))

    // def toSCCQ(wc: WhenClause): S2STry[AtomicPatterns] =
    //   this match
    //     case NodePattern(v) => 
    //     case EdgePattern(x, z, y) => 

  private def fgpToSCCQ(fgp: FullGraphPattern, wc: WhenClause): S2STry[AtomicPatterns] = Right(Nil) // TODO

  private def fgpToSCCQ(fgp: FullGraphPattern): S2STry[AtomicPatterns] = Right(Nil) // TODO

