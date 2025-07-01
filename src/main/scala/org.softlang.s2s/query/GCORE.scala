package org.softlang.s2s.query

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Showable
import de.pseifer.shar.core.Iri

import de.pseifer.shar.dl.NamedRole
import de.pseifer.shar.dl.NamedConcept

import org.softlang.s2s.core.S2STry
import org.softlang.s2s.core.Util
import org.softlang.s2s.core.isVariable
import org.softlang.s2s.core.UnsupportedQueryError
import org.softlang.s2s.core.UnconvertableShapeError
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.query.AtomicPattern
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.Vocabulary

import java.net.URLEncoder
import java.net.URLDecoder

/** Representation of a G-CORE (query) as match and construct clause.
  */
class GCORE(
    val template: GCORE.Construct,
    val pattern: GCORE.Match,
    val ret: Option[GCORE.Return] = None
) extends Showable:

  import GCORE._

  def show(implicit state: BackendState): String =
    "CONSTRUCT " ++ template.show ++ "\nMATCH " ++ pattern.show

  private val nodeEdgeVars: (Set[Var], Set[Var]) =
    pattern.fullGraphPattern
      .union(template.fullGraphPattern)
      .foldLeft((Set(), Set()))((acc, b) =>
        b match
          case GCORE.BasicGraphPattern.NodePattern(x) =>
            (acc._1.union(Set(x.toVar)), acc._2)
          case GCORE.BasicGraphPattern.EdgePattern(x, z, y) =>
            (acc._1.union(Set(x.toVar, y.toVar)), acc._2.union(Set(z.toVar)))
      )

  /** Node variables of the query. */
  val nodeVariables: Set[Var] = nodeEdgeVars._1

  /** Edge variables of the query. */
  val edgeVariables: Set[Var] = nodeEdgeVars._2

  /** Left-node variables of the query. */
  val leftNodeVariables: Set[Var] =
    pattern.fullGraphPattern
      .union(template.fullGraphPattern)
      .flatMap(b =>
        b match
          case GCORE.BasicGraphPattern.EdgePattern(x, _, _) => Set(x.toVar)
          case _                                            => Set()
      )

  /** Right-node variables of the query. */
  val rightNodeVariables: Set[Var] =
    pattern.fullGraphPattern
      .union(template.fullGraphPattern)
      .flatMap(b =>
        b match
          case GCORE.BasicGraphPattern.EdgePattern(_, _, x) => Set(x.toVar)
          case _                                            => Set()
      )

  /** Generate node patterns, given the graph pattern (fgp) the variables in
    * edge patterns (vars), and a mapping from variables to WhenClauses (may be
    * joined with Set/Remove clauses; see generateAtomic).
    */
  private def generateNodes(
      fgp: FullGraphPattern,
      vars: Set[Var],
      lok: Map[Variable, List[WhenClause]],
      realnode: Boolean = true
  ): Option[Set[AtomicPattern]] =
    // Iterate all NodePattern.
    Util
      .sequence(fgp.toList.map {
        case BasicGraphPattern.NodePattern(x) =>
          // Find all labels for x.
          val labels = lok.getOrElse(x, Nil).flatMap { x =>
            x match
              case WhenClause.HasLabel(_, l) => Some(l)
              case _                         => None
          }
          // Find all key-value pairs for x.
          val kvs = lok.getOrElse(x, Nil).flatMap { x =>
            x match
              case WhenClause.HasKeyValue(_, k, v) => Some((k, v))
              case _                               => None
          }
          // Find all key assertions (note: only for pattern).
          val keys = lok.getOrElse(x, Nil).flatMap { x =>
            x match
              case WhenClause.HasKey(_, k) => Some(k)
              case _                       => None
          }
          // Must be at least one label/k(v) for a valid SCCQ, or 'x' must be in vars.
          // TODO: Could (should!) be removed if GCORE.node sticks.
          if labels.isEmpty && kvs.isEmpty && keys.isEmpty && !vars.contains(
              x.toVar
            )
          then None
          else
            val vx = x.toVar
            // Generate VAC pattern for all
            // labels, key-values, and keys.
            Some(
              labels
                .map { l =>
                  AtomicPattern
                    .VAC(vx, l.toIri(node = nodeVariables.contains(vx)))
                }
                .concat(kvs.map { (k, v) =>
                  AtomicPattern.VPL(
                    vx,
                    k.toIri(node = nodeVariables.contains(vx)),
                    v.toIri
                  )
                })
                .concat(keys.map { k =>
                  AtomicPattern.VPV(
                    vx,
                    k.toIri(node = nodeVariables.contains(vx)),
                    Variable(k.keyname ++ "_" ++ x.name).toVar
                  )
                })
                .concat(
                  // TODO: NEW TEST ME
                  if realnode then List(AtomicPattern.VAC(vx, GCORE.node))
                  else Nil
                )
            )
        case BasicGraphPattern.EdgePattern(x, e, y) =>
          for
            n1 <- generateNodes(
              Set(BasicGraphPattern.NodePattern(x)),
              vars,
              lok
            )
            n2 <- generateNodes(
              Set(BasicGraphPattern.NodePattern(y)),
              vars,
              lok
            )
          yield n1.union(n2)
      })
      .map(_.flatten.toSet)

  /** Encode EdgePatterns in fgp to include out/in patterns and call NodePattern
    * for edge variables. Must be at least one edge label or property, else
    * None.
    */
  private def generateEdges(
      fgp: FullGraphPattern,
      lok: Map[Variable, List[WhenClause]]
  ): Option[Set[AtomicPattern]] =
    // Iterate all EdgePattern.
    Util
      .sequence(fgp.toList.map {
        case BasicGraphPattern.EdgePattern(x, e, y) =>
          // Find all labels for e.
          val labels = lok.getOrElse(e, Nil).flatMap { p =>
            p match
              case WhenClause.HasLabel(_, l) => Some(l)
              case _                         => None
          }
          // Find all key-value pairs for e.
          val kvs = lok.getOrElse(e, Nil).flatMap { p =>
            p match
              case WhenClause.HasKeyValue(_, k, v) => Some((k, v))
              case _                               => None
          }
          // Must be at least one for a valid SCCQ.
          // TODO: Could (should!) be removed if GCORE.edge sticks.
          if labels.isEmpty && kvs.isEmpty then None
          else
            val vx = x.toVar
            val vy = y.toVar
            val ve = e.toVar
            val s = Set(
              AtomicPattern.VPV(vx, nodeToEdgeIri, ve),
              AtomicPattern.VPV(ve, edgeToNodeIri, vy),
              // TODO: NEW TEST ME
              AtomicPattern.VAC(ve, GCORE.edge)
            )
            generateNodes(
              Set(BasicGraphPattern.NodePattern(e)),
              Set(),
              lok,
              realnode = false
            ).map(_.union(s))
        case BasicGraphPattern.NodePattern(_) => Some(Nil)
      })
      .map(_.flatten.toSet)

  private def generateAtomic(
      fgp: FullGraphPattern,
      clauses: Set[WhenClause]
  ): Option[AtomicPatterns] =
    // A lookup map from variables to related constraints (WhenClause).
    val lok =
      clauses.map(_.toVarTuple).groupMapReduce(_._1)(x => List(x._2))(_ ++ _)
    for
      // Generate the edges.
      edges <- generateEdges(fgp, lok)
      // Generate additional patterns from nodes.
      nodes <- generateNodes(fgp, edges.toList.variables, lok)
    yield edges.union(nodes).toList

  private def removeToFilter(r: RemoveClause): FilterPattern =
    r match
      case RemoveClause.RemoveKey(x, k) =>
        FilterPattern.notP(
          x.toVar,
          k.toIri(node = nodeVariables.contains(x.toVar))
        )
      case RemoveClause.RemoveLabel(x, l) =>
        FilterPattern.notC(x.toVar, l.toIri(nodeVariables.contains(x.toVar)))

  private def generateFilter(): Set[FilterPattern] =
    template._3.map(removeToFilter)

  /** Convert this query to a SCCQ. */
  val toSCCQ: Option[SCCQ] =
    for
      h <- sccqTemplate
      p <- sccqPattern
    yield
    // If this is a return query, use (partial) pattern as template.
    if ret.isDefined then
      SCCQ(
        template = p,
        pattern = p,
        eccq = Some(
          ECCQ(
            filter = generateFilter(),
            nodeVariables = nodeVariables,
            edgeVariables = edgeVariables
          )
        )
      )
    else
      SCCQ(
        template = h,
        pattern = p,
        eccq = Some(
          ECCQ(
            filter = generateFilter(),
            nodeVariables = nodeVariables,
            edgeVariables = edgeVariables
          )
        )
      )

  /** Generate the corresponding SCCQ template. */
  private def sccqTemplate: Option[List[AtomicPattern]] =
    generateAtomic(
      template.fullGraphPattern,
      // Merge clauses w.r.t. GCORE semantics.
      mergeClauses(
        from = pattern.when,
        set = template.set, // .union(extraClauses),
        remove = template.remove
      )
    )
    // Remove HasKey clauses (only relevant in patter).
    // .filter(x => x match
    //  case WhenClause.HasKey(_, _) => false
    //  case _ => true))

  /** Generate the corresponding SCCQ pattern. */
  private def sccqPattern: Option[List[AtomicPattern]] =
    generateAtomic(
      pattern.fullGraphPattern,
      // Simply include the full when clause (including HasKey).
      pattern.when
    )

  /** Validate this query. */
  def validate(): Boolean =
    // Node and edge variables must be disjoint.
    nodeVariables.intersect(edgeVariables).isEmpty

object GCORE:

  case class Variable(name: String, spliced: Boolean = false) extends Showable:
    def show(implicit state: BackendState): String = name

    /** Convert GCORE variable to SCCQ variable. */
    def toVar: Var = Var(name)

    def toIri(implicit scopes: Scopes): Iri = this.toVar.toIri

  object Variable:
    def fromVar(c: Var): Variable = Variable(c.v)
    def fromIri(i: Iri)(implicit scopes: Scopes): Variable = fromVar(
      Var.fromIriUnsafe(i)
    )

  case class Key(keyname: String) extends Showable:
    def show(implicit state: BackendState): String = "." ++ keyname

    /** Encode this key as IRI. */
    def toIri(node: Boolean): Iri =
      Iri
        .fromString(
          s"<${if node then Key.nodeIri else Key.edgeIri}/${keyname}>"
        )
        .toOption
        .get

  object Key:

    /** The IRI prefix of an edge Key. */
    def edgeIri: String = "https://github.com/softlang/s2s/ekey"

    /** The IRI prefix of a node Key. */
    def nodeIri: String = "https://github.com/softlang/s2s/nkey"

    /** Test if given Iri is an edge Key. */
    def isEdgeKey(i: Iri): Boolean =
      i.toString.contains(edgeIri)

    /** Test if given Iri is a node Key. */
    def isNodeKey(i: Iri): Boolean =
      i.toString.contains(nodeIri)

    /** Create a key from a compatible IRI. */
    def fromIri(i: Iri, node: Boolean): Key =
      val base = Iri
        .fromString(s"<${if node then Key.nodeIri else Key.edgeIri}/>")
        .toOption
        .get
      Key(i.retracted(base).get)

  case class Label(labelname: String, spliced: Boolean = false)
      extends Showable:
    def show(implicit state: BackendState): String = ":" ++ labelname

    /** Encode this label as IRI. */
    def toIri(node: Boolean): Iri =
      Iri
        .fromString(
          s"<${if node then Label.nodeIri else Label.edgeIri}/${labelname}>"
        )
        .toOption
        .get

  object Label:

    /** The IRI prefix of an edge Label. */
    def edgeIri: String = "https://github.com/softlang/s2s/elabel"

    /** The IRI prefix of a node Label. */
    def nodeIri: String = "https://github.com/softlang/s2s/nlabel"

    /** Test if given Iri is an edge Label. */
    def isEdgeLabel(i: Iri): Boolean =
      i.toString.contains(edgeIri)

    /** Test if given Iri is a node Label. */
    def isNodeLabel(i: Iri): Boolean =
      i.toString.contains(nodeIri)

    /** Create a label from a compatible IRI. */
    def fromIri(i: Iri, node: Boolean): Label =
      val base = Iri
        .fromString(s"<${if node then Label.nodeIri else Label.edgeIri}/>")
        .toOption
        .get
      Label(i.retracted(base).get)

  enum Value extends Showable:
    case IntValue(int: Int)
    case StringValue(string: String)
    case BooleanValue(bool: Boolean)
    case Spliced(name: String)

    def spliced: Boolean = this match
      case Spliced(_) => true
      case _          => false

    def show(implicit state: BackendState): String = this match
      case IntValue(i)     => i.toString
      case StringValue(s)  => s"\"$s\""
      case BooleanValue(b) => b.toString
      case Spliced(s)      => s"{$s}"

    /** Encode this value as IRI. */
    def toIri: Iri = this match
      case IntValue(i) =>
        Value.urlenc(Value.IRI_INT, i.toString)
      case StringValue(s) =>
        Value.urlenc(Value.IRI_STRING, s)
      case BooleanValue(b) =>
        Value.urlenc(Value.IRI_BOOL, b.toString)
      case Spliced(s) =>
        Value.urlenc(Value.IRI_VARIABLE, s)

  object Value:

    val IRI_INT = "https://github.com/softlang/s2s/int-"
    val IRI_STRING = "https://github.com/softlang/s2s/string-"
    val IRI_BOOL = "https://github.com/softlang/s2s/boolean-"
    val IRI_VARIABLE = "https://github.com/softlang/s2s/variable-"

    private def urlenc(prefix: String, unencoded: String): Iri =
      val url = URLEncoder.encode(
        unencoded,
        java.nio.charset.StandardCharsets.UTF_8.toString()
      )
      Iri.fromString(s"<${prefix}${url}>").toOption.get

    private def urldec(prefix: String, encoded: Iri): String =
      val base = Iri.fromString(s"<${prefix}>").toOption.get
      val enc = encoded.retracted(base).get
      URLDecoder.decode(enc, java.nio.charset.StandardCharsets.UTF_8.toString())

    def fromIri(i: Iri): Value =
      val look = i.toString
      if look.startsWith(s"<${Value.IRI_INT}") then
        Value.IntValue(urldec(Value.IRI_INT, i).toInt)
      else if look.startsWith(s"<${Value.IRI_STRING}") then
        Value.StringValue(urldec(Value.IRI_STRING, i))
      else if look.startsWith(s"<${Value.IRI_BOOL}") then
        Value.BooleanValue(urldec(Value.IRI_BOOL, i).toBoolean)
      else Value.StringValue(urldec(Value.IRI_VARIABLE, i))

  // type Query = BasicGraphQuery

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

  // SELECT

  case class Return(kinds: List[Kind])

  enum Kind:
    // Return the literal value of a property.
    case Property(v: Variable, k: Key)
    // Return a full node or edge.
    case Var(v: Variable)
    // Return the node or edge identity.
    case VarRaw(v: Variable)

  // CONSTRUCT (simplified)

  case class Construct(
      fullGraphPattern: FullGraphPattern,
      set: Set[SetClause],
      remove: Set[RemoveClause]
  ) extends Showable:
    def show(implicit state: BackendState): String = this match
      case Construct(fgp, s, r) =>
        val bs = fgp.map(_.show).mkString(", ")
        val ss =
          if s.nonEmpty then "\nSET " ++ s.map(_.show).mkString(" AND ") else ""
        val rs =
          if s.nonEmpty then "\nREMOVE " ++ r.map(_.show).mkString(" AND ")
          else ""
        bs ++ ss ++ rs

    // case ConstructWhen(fullGraphPattern: FullGraphPattern, booleanCondition: WhenClause)

  // CONSTRUCT (advanced)
  // type ConstructClause = Set[BasicConstruct]

  // enum BasicConstruct:
  //  case Construct(constructList: ConstructList)
  //  case ConstructWhen(constructList: ConstructList, eta: WhenClause)

  // type ConstructList = Set[ObjectConstruct]

  // enum ObjectConstruct:
  //  case NodeConstruct(x: Variable)
  //  case EdgeConstruct(x: Variable,  z: Variable, y: Variable)

  enum RemoveClause extends Showable:
    case RemoveKey(x: Variable, k: Key)
    case RemoveLabel(x: Variable, l: Label)

    def show(implicit state: BackendState): String = this match
      case RemoveKey(x, k)   => x.show ++ k.show
      case RemoveLabel(x, l) => x.show ++ l.show

    def asWhen: WhenClause = this match
      case RemoveKey(x, k)   => WhenClause.HasKey(x, k)
      case RemoveLabel(x, l) => WhenClause.HasLabel(x, l)

  enum SetClause extends Showable:
    case SetKeyValue(x: Variable, k: Key, v: Value)
    case SetLabel(x: Variable, l: Label)

    def show(implicit state: BackendState): String = this match
      case SetKeyValue(x, k, v) => s"${x.show}${k.show} = ${v.show}"
      case SetLabel(x, l)       => x.show ++ l.show

    def asWhen: WhenClause = this match
      case SetKeyValue(x, k, v) => WhenClause.HasKeyValue(x, k, v)
      case SetLabel(x, l)       => WhenClause.HasLabel(x, l)

  enum WhenClause extends Showable:
    case HasKey(x: Variable, k: Key)
    case HasKeyValue(x: Variable, k: Key, v: Value)
    case HasLabel(x: Variable, l: Label)

    def show(implicit state: BackendState): String = this match
      case HasKey(x, k)         => x.show ++ k.show
      case HasKeyValue(x, k, v) => s"${x.show}${k.show} = ${v.show}"
      case HasLabel(x, l)       => x.show ++ l.show

    /** Get the variable of this WhenClause. */
    def getVar: Variable = this match
      case HasKey(x, k)         => x
      case HasKeyValue(x, k, v) => x
      case HasLabel(x, l)       => x

    /** Get a mapping from getVar to this WhenClause. */
    def toVarTuple: (Variable, WhenClause) = (this.getVar -> this)

  /** Merge when, set and remove clauses according to GCORE semantics. */
  def mergeClauses(
      from: Set[WhenClause],
      set: Set[SetClause],
      remove: Set[RemoveClause]
  ): Set[WhenClause] =
    // Map set and remove to when (only relevant for syntax).
    val wset = set.map(_.asWhen)
    val wremove = remove.map(_.asWhen)
    // Take all when clauses.
    from
      // Remove when clauses that are overwritten by set clause (key-value).
      .filterNot { c =>
        c match
          case WhenClause.HasKeyValue(x, k, _) =>
            wset.exists { w =>
              w match
                case WhenClause.HasKeyValue(xi, ki, _) => x == xi && k == ki
                case _                                 => false
            }
          case _ => false
      }
      // Join with all set clauses.
      .concat(wset)
      // Filter remove clauses.
      .filterNot { c =>
        c match
          case WhenClause.HasKey(x, k) => wremove.contains(c)
          // HasKeyValue is also removedby HasKey (in remove clause).
          case WhenClause.HasKeyValue(x, k, v) =>
            wremove.contains(WhenClause.HasKey(x, k)) || wremove.contains(c)
          case WhenClause.HasLabel(x, k) => wremove.contains(c)
      }

  // MATCH
  case class Match(fullGraphPattern: FullGraphPattern, when: Set[WhenClause])
      extends Showable:

    def show(implicit state: BackendState): String = this match
      case Match(fgp, c) =>
        val bs = fgp.map(_.show).mkString(", ")
        val ws =
          if c.nonEmpty then "\nWHERE " ++ c.map(_.show).mkString(" AND ")
          else ""
        bs ++ ws

  type FullGraphPattern = Set[BasicGraphPattern]

  enum BasicGraphPattern extends Showable:
    case NodePattern(x: Variable)
    case EdgePattern(x: Variable, z: Variable, y: Variable)

    def show(implicit state: BackendState): String = this match
      case NodePattern(v)       => s"(${v.show})"
      case EdgePattern(x, z, y) => s"(${x.show})-[${z.show}]->(${y.show})"

  /** Internal IRI for 'out' edges. */
  val nodeToEdgeIri =
    Iri.fromString(s"<https://github.com/softlang/s2s/meta_nte>").toOption.get

  /** Internal role for 'out' edges. */
  val nodeToEdgeRole: NamedRole = NamedRole(nodeToEdgeIri)

  /** Internal IRI for 'in' edges. */
  val edgeToNodeIri =
    Iri.fromString(s"<https://github.com/softlang/s2s/meta_etn>").toOption.get

  /** Internal role for 'in' edges. */
  val edgeToNodeRole: NamedRole = NamedRole(edgeToNodeIri)

  /** Internal IRI for 'node'. */
  val node =
    Iri.fromString(s"<https://github.com/softlang/s2s/meta_node>").toOption.get

  /** Internal Concept for 'node'. */
  val nodeConcept: NamedConcept = NamedConcept(node)

  /** Internal IRI for 'edge'. */
  val edge =
    Iri.fromString(s"<https://github.com/softlang/s2s/meta_edge>").toOption.get

  /** Internal Concept for 'edge'. */
  val edgeConcept: NamedConcept = NamedConcept(edge)

  /** A vocabulary of things that should not be in a GCORE vocabulary. */
  val removeVoc: Vocabulary = Vocabulary(
    variables = Set(),
    concepts = Set(),
    properties = Set(),
    nominals = Set()
  )

  /** Convert a SHACL Shape to a SetClause. */
  def shapeToSetClause(shape: SHACLShape)(implicit
      scopes: Scopes
  ): S2STry[SetClause] =
    import de.pseifer.shar.dl._
    shape.axiom.c match
      case NamedConcept(ci) if ci.isVariable =>
        val v = Variable.fromIri(ci)
        shape.axiom.d match
          case NamedConcept(d) =>
            Right(
              SetClause.SetLabel(v, Label.fromIri(d, node = true))
            ) // TODO: What to select here?
          case Existential(NamedRole(r), NominalConcept(i)) =>
            Right(
              SetClause.SetKeyValue(
                v,
                Key.fromIri(r, node = true),
                Value.fromIri(i)
              )
            ) // TODO: ditto
          case _ =>
            Left(UnconvertableShapeError(shape))
      case _ =>
        Left(UnconvertableShapeError(shape))
