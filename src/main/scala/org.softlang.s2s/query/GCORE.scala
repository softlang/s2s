package org.softlang.s2s.query

object GCORE:
  case class Variable(name: String)
  case class Key(keyname: String)
  case class Label(labelname: String)
  
  enum Value:
    case IntValue(int: Int)
    case StringValue(string: String)
    case BooleanValue(bool: Boolean)

  type Query = BasicGraphQuery

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

  enum ConstructClause:
    case Construct(fullGraphPattern: FullGraphPattern)
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

  enum WhenClause:
    case HasKey(x: Variable, k: Key)
    case HasKeyValue(x: Variable, k: Key, v: Value)
    case HasLabel(x: Variable, l: Label)
    // TODO ...

  // MATCH
  enum MatchClause: 
    case Match(fullGraphPattern: FullGraphPattern)
    case MatchWhere(fullGraphPattern: FullGraphPattern, booleanCondition: WhenClause)

  type FullGraphPattern = Set[BasicGraphPattern]
    
  enum BasicGraphPattern:
    case NodePattern(x: Variable)
    case EdgePattern(x: Variable, z: Variable, y: Variable)

