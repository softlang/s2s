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
      case BasicGraphPattern.EdgePattern(x, e, y) => for 
        n1 <- generateNodes(Set(BasicGraphPattern.NodePattern(x)), vars, lok)
        n2 <- generateNodes(Set(BasicGraphPattern.NodePattern(y)), vars, lok)
      yield n1.union(n2)
    }).map(_.flatten.toSet)

  private def generateEdges(fgp: FullGraphPattern, lok: Map[Variable, List[WhenClause]]): Option[Set[AtomicPattern]] =
    // Iterate all EdgePattern.
    Util.sequence(fgp.toList.map {
      case BasicGraphPattern.EdgePattern(x, e, y) => 
        // Find all labels for e. 
        val labels = lok.getOrElse(e, Nil).flatMap { p => p match
          case WhenClause.HasLabel(_, l) => Some(l)
          case _ => None
        }
        // TODO: Incorporate key-value pairs.
        // Must be at least one for a valid SCCQ.
        if labels.isEmpty then None
        else 
          val vx = x.toVar
          val vy = y.toVar
          val ve = e.toVar
          val s = Set(
            // TODO: Use different namespaces for 'user' vs. 's2s' (in/out).
            AtomicPattern.VPV(vx, Label("out").toIri, ve),
            AtomicPattern.VPV(ve, Label("in").toIri, vy)
          )
          generateNodes(Set(BasicGraphPattern.NodePattern(e)), Set(), lok).map(_.union(s))
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

  /** Convert this query to a SCCQ. */
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

  type FullGraphPattern = Set[BasicGraphPattern]
    
  enum BasicGraphPattern extends Showable:
    case NodePattern(x: Variable)
    case EdgePattern(x: Variable, z: Variable, y: Variable)

    def show(implicit state: BackendState): String = this match
        case NodePattern(v) => s"(${v.show})"
        case EdgePattern(x, z, y) => s"(${x.show})-[${z.show}]->(${y.show})"

  private def fgpToSCCQ(fgp: FullGraphPattern, wc: WhenClause): S2STry[AtomicPatterns] = Right(Nil) // TODO

  private def fgpToSCCQ(fgp: FullGraphPattern): S2STry[AtomicPatterns] = Right(Nil) // TODO

