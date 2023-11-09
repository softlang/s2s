package org.softlang.s2s.query

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.Shar
import de.pseifer.shar.core.Showable

import org.softlang.s2s.core.S2STry
import org.softlang.s2s.core.UnsupportedQueryError

/** Representation of a G-CORE (query) as match and construct clause.
  */
class GCORE(
    val template: GCORE.ConstructClause,
    val pattern: GCORE.MatchClause
) extends Showable:

  val shar = Shar()
  def show(implicit state: BackendState = shar.state): String =
    "CONSTRUCT " ++ template.show ++ "\nMATCH " ++ pattern.show

  //def toSCCQ: SCCQ =
    // transform template to SCCQ template
    // transform pattern to SCCQ pattern

    // TODO
    //SCCQ(Nil, Nil)

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
    def show(implicit state: BackendState): String =
      name

  case class Key(keyname: String) extends Showable:
    def show(implicit state: BackendState): String =
      "." ++ keyname

  case class Label(labelname: String) extends Showable:
    def show(implicit state: BackendState): String =
      ":" ++ labelname
  
  enum Value extends Showable:
    case IntValue(int: Int)
    case StringValue(string: String)
    case BooleanValue(bool: Boolean)

    def show(implicit state: BackendState): String =
      this match
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

  type BasicGraphQuery = (ConstructClause, MatchClause)

  // CONSTRUCT (simplified)

  enum ConstructClause extends Showable:
    case Construct(fullGraphPattern: FullGraphPattern, set: List[SetClause], remove: List[RemoveClause])


    def show(implicit state: BackendState): String =
      this match
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

    def show(implicit state: BackendState): String =
      this match
        case RemoveKey(x, k) => "-" ++ x.show ++ k.show
        case RemoveLabel(x, l) => "-" ++ x.show ++ l.show

  enum SetClause extends Showable:
    case SetKeyValue(x: Variable, k: Key, v: Value)
    case SetLabel(x: Variable, l: Label)

    def show(implicit state: BackendState): String =
      this match
        case SetKeyValue(x, k, v) => s"${x.show}${k.show} = ${v.show}"
        case SetLabel(x, l) => x.show ++ l.show

  enum WhenClause extends Showable:
    case HasKey(x: Variable, k: Key)
    case HasKeyValue(x: Variable, k: Key, v: Value)
    case HasLabel(x: Variable, l: Label)

    def show(implicit state: BackendState): String =
      this match
        case HasKey(x, k) => x.show ++ k.show
        case HasKeyValue(x, k, v) => s"${x.show}${k.show} = ${v.show}"
        case HasLabel(x, l) => x.show ++ l.show

    // TODO ...

  // MATCH
  enum MatchClause extends Showable:
    case Match(fullGraphPattern: FullGraphPattern, conditions: List[WhenClause])

    def show(implicit state: BackendState): String =
      this match
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

    def show(implicit state: BackendState): String =
      this match
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

