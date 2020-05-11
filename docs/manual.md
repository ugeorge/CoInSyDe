# CoInSyDe: User Guide

CoInSyDe (**Co**de Target**in**g **Sy**stem **De**sign -- wordplay _coincide_) is a small component-based template expander which is meant to aid in the final stages of a generic computer-aided (embedded) system design flow: target code generation. It has been designed to be used by other tools, so if set up properly user interaction should be minimal. It is target-agnostic and not very "smart", meaning that it does not take any design decisions itself. Its only job is to select and expand predefined code templates in a manner which was already decided in a prior design process. 

Reaching CoInSyDe in a design flow should mean that the following design steps have already been performed and all the necessary information can be _extracted_ (and not assumed):

* the application has been formally modeled, validated and verified under the design assumptions. An application model should provide the core/kernel functions, for which _CoInSyDe requires semantic-equivalent translations in the target language_.
* the target platform has been designed and/or decided. Each target is associated with _a set of APIs, (hardware) abstraction layers and other similar parametrized mechanisms which CoInSyDe needs to instantiate_.
* the formal application model has been transformed and refined to an equivalent form, more appropriate to the target platform. For this model _CoInSyDe needs equivalent code templates implementing the formal interaction mechanisms in the target language_. 
* a design space exploration stage has been performed yielding a (maybe optimal) solution for mapping, scheduling, sharing, partitioning, etc. of the application on the target platform in order to satisfy the design constraints. Such a result would provide CoInSyDe with *information on the order of instantiation and execution of different functions, which particular API calls are needed for scheduling, communication, etc.* 

All this information should be compiled into a set of exchange files used as input for CoInSyDe. This user guide shows how to set up a CoInSyDe project and workspace, and how the inputs need to look like in order to obtain compilable target code. But before we proceed, repeat after me (multiple times if needed):

> CoInSyDe is _not_ meant to be used by humans. It is a tool for machines, invoked as part of a (much larger) CAD process. 

If at any time in this guide the tool design and its inputs seem strange, awkward, too verbose or non-intuitive, re-read this statement. Of course, this assumes that a (fully-automated) CAD process actually exists. It is still up to (human) engineers to set up a workspace properly in order to give CoInSyDe enough to work with. Fortunately, there are a couple of features to help in designing and debugging such an environment.

One last thing before we start, CoInSyDe has been developed as part of the [ForSyDe](https://forsyde.github.io) ecosystem and is, obviously, being used in the context of heterogeneous embedded multi-processor systems. Apart from that there is nothing that binds CoInSyDe to ForSyDe or this particular application area. CoInSyDe treats target code as plain text, and its AST only defines _components_ and _interfaces_, so any methodology based on these concepts should be fit to use CoInSyDe as backend code generator. Currently there is only one target family language supported, namely C, but I am working on a VHDL backend and plan to extend the tool further. 

## Prerequisites

Check the [README](../README.md) file on how to install the CoInSyDe tool and its dependendies. For a complete user experience while going through this document, I recommend that you also have installed:

* a compatible version of [Pandoc](https://pandoc.org/) accessible from the command prompt. To check compatibility run both `coinsyde --version` and `pandoc --version` and see if they are compiled with the same major version of `pandoc-types`.

		$ coinsyde -v
		coinsyde-0.2.1.0
		CoInSyDe -- Code Targeting System Design -- version 0.2.1.0
		(c) 2019-2020 George Ungureanu
		Using 'pandoc-types' version 1.20
		------------------------------ ^
		(...)
		
		$ pandoc -v
		pandoc 2.9.2.1
		Compiled with pandoc-types 1.20, texmath 0.12.0.1, skylighting 0.8.3.2
		---------------------------- ^
		(...)

* a [DOT graph](https://www.graphviz.org/) plotter/viewer. I have used and could recommend `xdot`.

None of the above is necessary for the core functionality of CoInSyDe, but rather for extra documentation features.

## Quick Start

This section goes through a hands-on tutorial using a predefined toy project found in [example](../example). The purpose is to get you, the potential tool developper, used to the features of CoInSyDe. 

**OBS:** the CoInSyDe tool is still experimental and under development. Some of this documentation might be outated by the time you read this. I have marked all the features that might be updated in the future with pointers where to track the eventual changes.

### Using the CLI tool

When installed properly according to the [installation instructions](../README.md), you should be able to run the command `coinsyde`. The first thing you should do is to check what options your version has to offer:

	$ coinsyde --help

In order to run any instance, CoInSyDe needs to load a global configuration found in its own user data directory. If you haven't done so already, run 

	$ coinsyde --global-init
	
otherwise any attempt to execute `coinsyde` would result in an error message. More details on how the configuration affects the tool will be discussed in the [Library Management](#library-management) section.

### Setting up a workspace

The `coinsyde` command can be called from anywhere _under_ a workspace root. A workspace is a directory structure holding a suite of projects sharing libraries and a common configuration file. To get familiar with a workspace structure create a template workspace now in an empty folder, e.g.:

	$ mkdir test-workspace
	$ cd test-workspace
	$ coinsyde --new
	
This will create a couple of empty folders where libraries and project files will be found, as well as a template configuration file `coinsyde.yaml`. Open it, study it and modify it accordingly. Each entry is documented inline and the usage should be self-explanatory.

Skimming through a template workspace should be enough to get a hang of how projects need to look like. More details are covered in the section [Advanced Topics](#advanced-topics).

### Running your first project

CoInSyDe comes with an example workspace containing a toy project called `toy1` which exposes most of the tool's features and usage. For the next step you can remove the `test-workspace` made during the last section, and instead `cd` into the [`examples`](../examples) folder. Please take some time to study it: open the configuration file, project files, libraries, and try to piece together what you find there against the pieces of information in this tutorial.

**OBS:** at this point you should be quite familiar with [YAML](https://yaml.org/spec/1.2/spec.html), which is the main exchange format used in CoInSyDe. If you don't, check out this [quick tutorial](https://learnxinyminutes.com/docs/yaml/) before you proceed.

#### The design

Open the main project input file [`examples/proj/toy1/toy1.yaml`](../examples/proj/toy1/toy1.yaml). At this point it is not very insightful and does not make much sense, but have it opened in a window close-by anyway. This file in fact describes the internals of the _refined_[^1] process network given in the following listing as [ForSyDe-Atom](https://forsyde.github.io/forsyde-atom/) model:

```haskell
import ForSyDe.Atom.MoC.SDF as SDF
import ForSyDe.Atom.Skeleton.Vector as V

coef = V.vector [0,1,3,2,1]

toySystem :: SDF.Signal Int -> SDF.Signal Int
toySystem = SDF.comb11 10 10 (V.fromVector . mav . V.vector)
  where
	mav  i   = V.shiftFarm21 fred i coef
	fred i c = V.farm2ReduceI mulacc 0 i c
	mulacc acc i1 i2 = acc + i1 * i2
```
This system represents a moving-average (MAV) algorithm applied on chuncks of 10 elements from infinite streams of integers. The streaming behavior is modeled as a combinational synchronous dataflow (SDF)[^2] process, which consumes 10 tokens and produces 10 tokens. The MAV algorithm is described as a composite pattern made of a shift-farm (i.e. _moving_), which incrementally applies a farm-reduce (i.e. _average_) on pairs of chunked input and static coefficient vectors. This system would look like in the following pictures:

![The `toySystem` process and its testbench](assets/toy1.svg)

![The `mav` vector function as composition of skeletons](assets/toy1-mav.svg)

Notice that the input file [`toy1.yaml`](../examples/proj/toy1/toy1.yaml) describes the _full testbench_ as its top module `main`, not just the design-under-test (DUT) `toySystem`. This means the "somewhere along the design flow" the decision was take so that "clouds" _Signal Source_ and _Signal Sink_ are replace with some appropriate target IO functions (in this case the `scanf`- and `prinft`-based patterns).

**OBS:** As said in the beginning, ForSyDe-Atom, or any ForSyDe model for that matter, is not a mandatory input for CoInSyDe. I merely used this application model for understanding purpose only. Any methodology based on formally-defined (higher-order) compoments is equally appropriate for providing CoInSyDe input descriptions.

[^1]: it is refined because it has been subjected to a series of semantic-preserving transformations in order to reach this form, which is more suitable for implementation towards imperative sequential code. FOr the documented refinement process, check the case study in [_(not published yet)_]().

[^2]: [[Lee and Seshia, 2017, Cha. 6, p. 152--156]](https://ptolemy.berkeley.edu/books/leeseshia/releases/LeeSeshia_DigitalV2_2.pdf)

#### Patterns. Components. Types

#### Bindings

#### Generating code

## Advanced Topics

### Library Management

### Input Files

### Templates Language

