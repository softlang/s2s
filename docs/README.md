# Tutorial: How to get Shapes from Shapes

This tutorial will give a step-by-step introduction to the *From Shapes to Shapes* method. Input files (queries and shapes) for all examples can be found in the [examples](ex/tut) folder. The tutorial will instruct you to execute examples. To this end, make sure to read the main [README](../README.md), which includes details on how to use the `s2s` tool. In this tutorial, we will refer to `s2s` for executing examples and expect readers to be familiar with how to run the tool on their platform. Generally, you will need to input `./s2s`, `.\s2s.bat`, or `.\bin\s2s.bat` depending on your system and installation method. Using `s2s --help` generates additional helpful information about using `s2s`.

- [Introduction: The Problem](#introduction-the-problem)
- [Namespaces](#namespaces)
- [Reducing the Problem](#reducing-the-problem)
- [Constructing Axioms and Entailing Shapes](#constructing-axioms-and-entailing-shapes)

## Introduction: The Problem

Conside a SPARQL `CONSTRUCT` query `q` that selects all `x` that are instances of *Person* and all `y` that are instances of *Agent*, and then constructs this subgraph as its result graph. Note, that we assume a default prefix `:` in all examples. (This prefix is also defined by default when using the implementation.)

```sparql
CONSTRUCT {
    ?x a :Person .
    ?y a :Agent
} WHERE {
    ?x a :Person .
    ?y a :Agent
}
```

or, more succinctly (note: the following syntactic sugar is not yet supported by S2S):

```sparql
CONSTRUCT WHERE {
    ?x a :Person .
    ?y a :Agent
}
```

Now consider that the input graph for this query -- that is, the graph that this query is executed against -- conforms to a set of SHACL shapes S_in = { `:Person` ⊑ `:Agent` }. In this tutorial, we denote SHACL shapes as description logics axioms. More details on this equivalency can be found in our paper. Essentially, this means the left-hand side of ⊑ (subsumed-by) is the target query, and the right-hand side is the constraint. Therefore, the single shape in `S_in` states that every person is an agent, meaning that all instances of the class *Person* are required to also be instances of the class *Agent*.

In *From Shapes to Shapes* we ask the question: Which set of output shapes `S_out` applies to the output graphs (i.e., the graphs constructed by a SPARQL `CONSTRUCT` query), given that we know about the query and a set of input shapes, but we do not know about which concrete graph instances may be encountered in the future. More formally, therefore, we want a function `s2s : (S_in, q) → S_out`. The `s2s` tool implements precisely this function, where q1 is the query and S1 the set of shapes from the previous example, so that

```sh
> s2s docs/ex/tut/q1.sparql docs/ex/tut/S1.shacl
```

produces the single shape `:Person` ⊑ `:Agent`.

It might not be terribly surprising, that the result shapes consist of a single shape, `:Person` ⊑ `:Agent`. Let us establish why this is the case, anyways: First, we know that the query matches all instances of *Person* and *Agent*, and binds them to `x` and `y`, respectively. Due to the input shape `:Person` ⊑ `:Agent`, the set of all bindings that `x` takes is a subset of the set of all bindings that `y` takes, since every *Person* must also be an *Agent*. Next, the query constructs a new graph from `x` and `y`, using the same concepts *Person* and *Agent*. Therefore, the same subset relationship is transferred to the new graph, such that `:Person` ⊑ `:Agent` holds.

Let us next consider a slightly more interesting query (q2) and the same shapes (S2 = S1) as before

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

```sh
> s2s docs/ex/tut/q2.sparql docs/ex/tut/S2.shacl
```

which produces `:Agent` ⊑ `:Person` and `:Person` ⊑ `:Agent`. Why is this the case? In the WHERE clause of the query, `x` is constrained more tightly than before. This does not, however, change the subset relationship that can be established between `x` and `y`. The CONSTRUCT template, on the other hand, includes now the triple pattern `?x a :Person`, which does have consequences regarding the result shapes: Since we establish that `y` is both *Person* and *Agent* in the new graph, and we know (as before) that `x` is a subset of `y` anyways, both inclusions `:Agent` ⊑ `:Person` and `:Person` ⊑ `:Agent` can be inferred to hold for any possible result graph of the query, as long as, of course, the respective input graph conforms to the input shapes.

## Namespaces

Consider again the previous query and the same input shapes as before (namely S_in = { `:Person` ⊑ `:Agent` }). Let us now consider a different question: Do the various occurrences of the symbols (e.g., *Person*) refer to the same sets of individuals, as it was the case in the previous example? For clarity, let us index the different uses of symbols, e.g., distinguish *Person* into *Person_0*, *Person_1*, and *Person_2*, as follows:

{ `:Person_0` ⊑ `:Agent_0` }

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

Let us now investigate the extensions of the concepts *Person_0*, *Person_1*, and *Person_2*. The extension of *Person_1* includes only individuals matched by the query pattern, which requires *Teacher_1* as well. Thus, *Person_1* may be subsumed by and may be unequal to *Person_0*. Similarly, *Person_2* now also includes all bindings of variable `y`, unlike *Person_0* or *Person_1*. In simpler terms, the three differing occurrences of concept names (i.e., classes) do not refer to the same concepts, with respect to their extensions.

We can use S2S in order to visualize this as well, by passing the `--debug` flag.

```sh
> s2s --debug docs/ex/tut/q2.sparql docs/ex/tut/S2.shacl
```

## Reducing the Problem

How can we infer this full set of shapes? We reduce the problem by considering a subset of SHACL shapes that can be enumerated, and that consists only of shapes that are relevant to the problem at hand. This relevance follows from consideration of the vocabulary of the query (and input shapes), and a trivial restriction to finite candidates is the subset of Simple SHACL we define in our main paper. However, the same approach can be used when considering any finite set of shapes, be it constructed by some other restrictions or generated from a heuristic. For such a finite set of candidate shapes, the problem can be reduced to filtering the candidates. That is, we consider the simpler problem `s2s-test : (S_in, q, s) → {YES, NO}`, where given a set of input shapes, a query, and a candidate shape, we decide whether this candidate is an element of the result shapes, that is, guaranteed to validate all possible output graphs.

The CLI tool exists mainly for exploring the basics of our method alongside the paper, using Simple SHACL; however, when using S2S as a library, one can obtain the complete set of inferred axioms, and also supply more general ALCHOI-based SHACL shapes as input. Then, one can verify for any given shape whether it holds on all outputs. Such candidate shapes could then be generated heuristically, semi-manually, or from other information entirely depending on the use case -- unlocking the full power of the *Shapes to Shapes* method. Indeed, one could also generate all relevant ALCHOI-based SHACL shapes to test them, though this would be quite inefficient and possibly not very helpful.

## Constructing Axioms and Entailing Shapes

Consider again the `--debug` flag

```sh
> s2s --debug docs/ex/tut/q2.sparql docs/ex/tut/S2.shacl
```

The output includes a set of axioms (KB) constructed from the given inputs (query and input shapes) in order to filter candidate shapes. The condition of this filter is essentially a check for entailment of the (DL-encoded) candidate shape (s) by the constructed set of axioms, i.e., answering: Does KB ⊢ s hold? Our full paper includes the complete definition of how this set of axioms is constructed, and the extended version features the proof of soundness. Here, we want to only discuss the basic intuition, using again this relatively simple example query.

All axioms listed are part of a single T-Box. Consider first the axiom labelled S_in, and note the input namespace encoding used through the suffix •0. Here, the token '•' (which can also be customized) is used to avoid accidental naming collisions in real-world input data, and '0' indicates the input namespace.

> S_in = Person•0 ⊑ Agent•0

More interesting are the axioms inferred for the WHERE clause (also called the *pattern*) of the CONSTRUCT query. The first part, labelled with 'step 1', defines the concept names (in the intermediate or WHERE-clause namespace •1) in terms of filtering the input-namespace (•0) concept names with the respective query variables -- or rather, with concept names that are used as stand-ins for the query variables, using the suffix •-1. That is, axiom Agent•1 ≡ (Agent•0)⊓(?y•-1) says that Agent•1 is the same as Agent•0 filtered by some concept ?y•-1, which essentially includes the bindings of variable `y`.

> CWA(q.P), step 1 =
>   Agent•1 ≡ (Agent•0)⊓(?y•-1),
>   Person•1 ≡ (Person•0)⊓(?x•-1),
>   Teacher•1 ≡ (Teacher•0)⊓(?x•-1)

How is ?y•-1 defined, then? Well, the axioms labelled with 'step 3' define ?y•-1 in terms of the constraints arising from the WHERE-clause: Agent•0 ≡ ?y•-1, though expressed as both inclusions, since sometimes one direction can not be ensured; see the paper for more details. Perhaps more interesting is the definition for variable `x`, namely (Person•0)⊓(Teacher•0) ≡ ?x•-1. Here, ?x•-1 is defined as the intersection of the two concept names that occur in the patterns of the WHERE-clause, since bindings for `x` are required to be instances of both *Person* and *Teacher*.
 
> CWA(q.P), step 3. =
>   (Person•0)⊓(Teacher•0) ⊑ ?x•-1,
>   Agent•0 ⊑ ?y•-1,
>   ?x•-1 ⊑ (Person•0)⊓(Teacher•0),
>   ?y•-1 ⊑ Agent•0

Finally, we define concept names of the output namespace (•2) in terms of the query variables that partake in their construction, only ?y•-1 for Agent•2, and the union of both ?x•-1 and ?y•-1 for Person•2.

> CWA(q.H), step 2. =
>   Agent•2 ≡ ?y•-1,
>   Person•2 ≡ (?x•-1)⊔(?y•-1)

With these axioms, we can then filter the set of candidate shapes and obtain the shapes listed in S_out as the only ones entailed by the axioms above.

> S_out =
>   Agent•2 ⊑ Person•2,
>   Person•2 ⊑ Agent•2

