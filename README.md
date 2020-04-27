# dataflower (data + 🌼)

[![Build Status](https://travis-ci.org/doublecrowngaming/dataflower.png)](https://travis-ci.org/doublecrowngaming/dataflower)

Timely Dataflow for Haskell

Dataflower is an implementation of a [Timely Dataflow](https://timelydataflow.github.io/timely-dataflow/)-like system written in pure Haskell. The design uses the original [2013 Naiad paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2013/11/naiad_sosp2013.pdf) from Microsoft Research as its design basis.

Dataflower leaves out many of Timely Dataflow and Naiad's capabilities for the time being. The focus is first to ensure the framework is easy to use and correct. Scalability will come after that.

## What is "Dataflow"?

### Dataflow Programming
Dataflow programming, sometimes called *datastream* programming, is a paradigm where programs are constructed as directed graphs. Communication occurs along edges, and computation happens at vertices. The communication code is taken care of by the runtime system, and the programmer is responsible for building each vertex as a black box, and then connecting together the flow graph.

Conventionally the flow graph must be acyclic. Microsoft Research's Naiad project took dataflow several steps further, and designed a system that allowed for cyclic directed graphs. This makes it possible to construct dataflow systems with feedback or iteration.

### Dataflow vs. Streaming
Why not use Conduit, Streamly, Pipes, or so on? Streaming frameworks concern themselves with the world of I/O. Data comes from a source, it is transformed through a pipeline, and is passed out a sink. Their goal is to make writing robust *linear* pipelines easy. They do that very well.

Dataflow is concerned exclusively with the world of computation. Input is multiplexed through a single source node, and then flows through the computational graph in parallel.

Here's an example to show what is easy in Dataflow that would be rather unnatural in a streaming framework:

```
newtype Mean a = Mean a deriving (Eq, Show)

arithMean :: Edge (Mean Int) -> Dataflow (Edge Int)
arithMean next = ...

geomMean :: Edge (Mean Int) -> Dataflow (Edge Int)
geomMean next = ...

fanout :: [Edge a] -> Dataflow (Edge a)
fanout nexts = ...

computation :: TVar (Mean Int) -> TVar (Mean Int) -> TVar [Int] -> Dataflow Int
computation arithTV geomTV seenTV = do
  arithOutput <- outputTVar id arithTV
  geomOutput  <- outputTVar id geomTV
  seen        <- outputTVar (:) sumTV

  arith <- arithMean arithOutput
  geom  <- geomMean geomOutput

  fanout [arith, geom, seen]
```

Dataflow output gets placed in shared memory. The first `TVar` will contain the arithmetic mean of the inputs provided to `computation`. The second `TVar` will contain the geometric mean. The third `TVar` will contain the list of all the inputs in the order in which they were seen.

This last one is important: since dataflows are computational graphs, it's completely natural to "skip" a "stage" -- `seen` doesn't require anything special to get a copy of the input as-is because we don't have a single pipeline.

Now imagine that we also want to keep track of the list of arithmetic residues -- that is, the difference between the current input and the most recent arithmetic mean.

It's true we could do this by modifying `arithMean` so now instead of doing one thing well, it does two things adequately. The cost is greater complexity and more elaborate test needs.

We could also create a `residue :: Edge Int -> Dataflow (Edge (Mean Int), Edge Int)` vertex that keeps the most recent mean and passes on the absolute difference between its input and that mean. But notice something: nothing we just said has anything to do with _arithmetic_ means. This `residue` vertex is generic and can be used with the output of any averaging vertex.

## Essential Concepts

Dataflow programming in Haskell can be thought of as similar to continuation passing. Unlike normal CPS where the continuation is passed directly to a function, in Dataflow programming a typed pointer is passed and the continuation is invoked using the `send :: Edge i -> Timestamp -> i -> Dataflow ()` operator.

### Timestamps

The `Timestamp` is what makes Timely Dataflow timely. Timestamps *causally separate* collections of inputs. If `t1` and `t2` are each a `Timestamp`, then if `t2 > t1` *any* data associated with `t2` could have been the result of `t1`, and *no* data associated with `t1` could have been the result of `t2`.

Timestamps, rather than being scalars, are vector-valued in timely dataflow. Entering a feedback loop in a timely dataflow graph adds a dimension to the `Timestamp`, and exiting it removes a dimension. This happens automatically and is opaque to the programmer.

Taken together, these two attributes allow timely dataflow to identify the next inputs to process to ensure the system makes progress towards a result.

### Vertices & Edges

Computation happens at a vertex. Much of your work in Dataflower will consist of writing custom vertices to model your computations. The complexity varies depending upon how rich the behavior of your vertex is. None of that richness is visible outside your vertex though -- all your upstream knows is that it has an `Edge a` to `send` to. Your downstream doesn't even know you exist.

An `Edge a` is totally opaque. You get one when you define a vertex, and the only thing you can do is `send` to it.

## Support

Assistance is available via the Github issue tracker for this project or in #Haskell on [FP Slack](functionalprogramming.slack.com). Paid support is available -- contact ops@doublecrown.co for details.

## Features

- [x] Acyclic graph support
- [x] Single threaded execution
- [ ] Persistable checkpoints
- [ ] Cyclic graph support
- [ ] Multithreaded execution
- [ ] Distributed execution
