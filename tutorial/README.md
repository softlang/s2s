# Tutorial: From Shapes to Shapes

This tutorial will give a step-by-step introduction to the *From Shapes to Shapes* method, as well as instructions for the usage of the `s2s` implementation. Input files (queries and shapes) for all examples can be found in the [examples](examples) folder. The tutorial will instruct you to execute examples. To this end, make sure to read the main [README](../README.md), which includes details on how to use the `s2s` tool.

## Quick Start and Further Reading

Please see the main [README](../README.md) document for general notes on running `s2s`. In this tutorial, we assume usage of the `s2s` utility script (i.e., a linux or macOS environment). For Windows, remember to replace `s2s` with `s2s.bat`. Pointers regarding further reading can be found in the primary document as well.

## Introduction: The Problem

Conside a SPARQL `CONSTRUCT` query `q`, that selects all `x` that are instances of *Person*, and all `y` that are instances of *Agent*, and then constructs this subgraph as its result graph. Note, that we assume a default prefix `:` in all examples; this prefix is also defined by default when using the implementation.

```sparql
CONSTRUCT {
    ?x a :Person .
    ?y a :Agent
} WHERE {
    ?x a :Person .
    ?y a :Agent
}
```

Now consider that the input graph for this query -- that is, the graph that this query is executed against -- conforms to a set of SHACL shapes `S_in = { :Person ⊑ :Agent }`. In this tutorial, we denote SHACL shapes as description logics axioms (more details on this equivalency can be found in our paper, see the [Further Reading](#quick-start-and-further-reading) section). Essentially, this means the left-hand side of `⊑` is the target query, and the right-hand side is the constraint. Therefore, the single shape in `S_in` states that every person is an agent, meaning that all instances of the class *Person* are required to also be instances of the class *Agent*.

In *From Shapes to Shapes* we ask the question: Which set of output shapes `S_out` applies to the output graphs (i.e., the graphs constructed by a SPARQL `CONSTRUCT` query), given that we know about the query and a set of input shapes, but we do not know about which concrete graph instances may be encountered in the future.

More formally, therefore, we want a function `s2s : (S_in, q) → S_out`. The `s2s` tool implements precisely this function:

```
> s2s example/q1.sparql example/S1.shacl

:Person ⊑ :Agent
```

It might not be terribly surprising, that the result shapes are `{ :Person ⊑ :Agent }`. Let us reconstruct why this is the case, anyways: First, we know that the query matches all instances of *Person* and *Agent*, and binds them to `x` and `y`, respectively. Due to the input shape `:Person ⊑ :Agent`, the set of all bindings that `x` takes is a subset of the set of all bindings that `y` takes, since every *Person* must also be an *Agent*. Next, the query constructs a new graph from `x` and `y`, using the same concepts *Person* and *Agent*. Therefore, the same subset relationship is transferred to the new graph, such that `:Person ⊑ :Agent` holds.

Let us next consider a more interesting Problem:

```sparql
CONSTRUCT {
    ?x a :Person.
    ?y a :Person.
    ?y a :Agent.
} WHERE {
    ?x a :Person .
    ?x a :Teacher .
    ?y a :Agent.
}
```

Consider the above query and the same input shapes as before (namely `S_in = { :Person ⊑ :Agent }`). Instead of thinking about the result shapes, let us first consider a different question: Do the various occurrences of the symbols (e.g., *Person*) refer to the same sets of individuals, as it was the case in the previous example? For clarity, let us index the different uses of symbols, e.g., distinguish *Person* into *Person_0*, *Person_1*, and *Person_2*.

`{ :Person_0 ⊑ :Agent_0 }`

```sparql
CONSTRUCT {
    ?x a :Person_2 .
    ?y a :Person_2 .
    ?y a :Agent_2
} WHERE {
    ?x a :Person_1 .
    ?x a :Teacher_1 .
    ?y a :Agent_1
}
```

Let us now investigate the extensions of the concepts *Person_0*, *Person_1*, and *Person_2*. The extension of *Person_1* includes only individuals matched by the query pattern, which requires *Teacher_1* as well. Thus, *Person_1* may be subsumed by and may be unequal to *Person_0*. Similarly, *Person_2* now also includes all bindings of variable `y`, unlike *Person_0* or *Person_1*.

## Reducing the Problem
