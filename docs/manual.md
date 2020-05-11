# CoInSyDe: User Guide

CoInSyDe (**Co**de Target**in**g **Sy**stem **De**sign -- wordplay _coincide_) is a small component-based template expander which is meant to aid in the final stages of a generic computer-aided (embedded) system design flow: target code generation. It has been designed to be used by other tools, so if set up properly user interaction should be minimal. It is target-agnostic and not very "smart", meaning that it does not take any design decisions itself. Its only job is to select and expand predefined code templates in a manner which was already decided in a prior design process. 

Reaching CoInSyDe in a design flow should mean that the following design steps have already been performed and all the necessary information can be _extracted_ (and not assumed):

* the application has been formally modeled, validated and verified under the design assumptions. An application model should provide the core/kernel functions, for which _CoInSyDe requires semantic-equivalent translations in the target language_.
* the target platform has been designed and/or decided. Each target is associated with _a set of APIs, (hardware) abstraction layers and other similar parametrized mechanisms which CoInSyDe needs to instantiate_.
* the formal application model has been transformed and refined to an equivalent form, more appropriate to the target platform. For this model _CoInSyDe needs equivalent code templates implementing the formal interaction mechanisms in the target language_. 
* a design space exploration stage has been performed yielding a (maybe optimal) solution for mapping, scheduling, sharing, partitioning, etc. of the application on the target platform in order to satisfy the design constraints. Such a result would provide CoInSyDe with *information on the order of instantiation and execution of different functions, which particular API calls are needed for scheduling, communication, etc.* 

All this information should be compiled into a set of exchange files used as input for CoInSyDe. This user guide shows how to set up a CoInSyDe project and workspace, and how the inputs need to look like in order to obtain compilable target code. But before we proceed, repeat after me (multiple times if needed):

> CoInSyDe is _not_ meant to be used by humans. It is a tool for machines, invoked as part of a (much larger) CAD process. 

If at any time in this guide the tool design and its inputs seem strange, awkward, too verbose or non-intuitive, re-read this statement and you will understand _why_ it needs to be so. Of course, this assumes that a (fully-automated) CAD process actually exists. It is still up to (human) engineers to set up a workspace properly in order to give CoInSyDe enough to work with. Fortunately, there are a couple of features to help in designing and debugging such an environment.

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

![The `toySystem` process and its testbench](assets/toy1.png)

![The `mav` vector function as composition of skeletons](assets/toy1-mav.png)

Notice that the input file [`toy1.yaml`](../examples/proj/toy1/toy1.yaml) describes the _full testbench_ as its top module `main`, not just the design-under-test (DUT) `toySystem`. This means the "somewhere along the design flow" the decision was take so that "clouds" _Signal Source_ and _Signal Sink_ are replace with some appropriate target IO functions (in this case the `scanf`- and `prinft`-based patterns).

**OBS:** As said in the beginning, ForSyDe-Atom, or any ForSyDe model for that matter, is not a mandatory input for CoInSyDe. I merely used this application model for understanding purpose only. Any methodology based on formally-defined (higher-order) compoments is equally appropriate for providing CoInSyDe input descriptions.

[^1]: it is refined because it has been subjected to a series of semantic-preserving transformations in order to reach this form, which is more suitable for implementation towards imperative sequential code. FOr the documented refinement process, check the case study in [_(not published yet)_]().

[^2]: [[Lee and Seshia, 2017, Cha. 6, p. 152--156]](https://ptolemy.berkeley.edu/books/leeseshia/releases/LeeSeshia_DigitalV2_2.pdf)

#### Patterns. Components. Types

Study more closely the file [`toy1.yaml`](../examples/proj/toy1/toy1.yaml). Notice that there are two types of entries: `pattern` entries and `type` entries. But what are these? At this point it is good to introduced the two main categories of AST elements stored and manipulated by CoInSyDe:

* _types_ contain information which will help CoInSyDe synthesize code for target-relevant _data types_. These are specified as `type` YAML entries. 
* _components_ contain all the necessary information about what will become _functions_ in the target artifact. This includes interfaces (ports, parameters), references to other components, bindings, specific requirements, and/or target code. Components are themselves grouped into three kinds:
  * _native functionals_ are components that will be used as-is in the synthesized code. They _need to_ specify a set of interfaces each, so that they can be referenced by other components (e.g. as function calls), but their code is treated as "plain text", and is copy-pasted or `#include`d into the target artifact without any manipulation done. They are specified as `native` YAML entries.
  * _template functionals_ are components that enable the manipulation and generation of specific system setups as target code. They _need to_ specify target code written in the [Ginger](https://ginger.tobiasdammers.nl/) templating language[^3], which is a markup language that grants access to information stored in a component's interfaces. They _might_ specify a set of interfaces themselves, but mainly for documentation purposes. They are specified as `template` YAML entries.
  * _patterns_ are "wrappers" around template functionals. They do not contain template code themselves, but rather _import_ it from an already-defined template component. However they _need to_ define a valid set of interfaces so that they can be referenced by other components or even reference themselves other components. They are specified as `pattern` YAML entries.

But why do we need so many different components? Well, mainly for practical reasons, since internally they really are just _components_. Without getting too much into detail, here is a plausible scenario in which CoInSyDe is being used (see [Advanced Topics](#advanced-topics) for more):

* a number of _platform experts_ would build "correct" libraries of types (for primitive or standard data types), parametrizable template components (for each supported pattern) and native components (usually for API library functions) specific to each target platform. 
* a _system design flow_ would provide a custom-tailored instantiation of a modeled application, using only supported patterns ("filled in" with the right parameters), custom types (complex or parametric structures with equivalents in the target language), and a set of native components (pre-compiler, extracted or provided by the design flow for each model kernel function). 

So what you see in the [`proj/toy1`](../examples/proj/toy1/) project folder is in fact the input specfications generated by a previous design flow. As for the rest of the inputs (i.e. under [`libs`](../examples/libs/) and [`usrlibs`](../examples/usrlibs/)), they were provided by the "platform experts" and are defining the target platform. Let us check more closely the `mav_1` component in [`toy1.yaml`](../examples/proj/toy1/toy1.yaml):

```yaml
pattern:
# (...)
- name: mav_1
  type: Skeleton.ShiftFarm
  port:
  - {name: in1,   kind: iarg.1, type: ArrInt10}
  - {name: out1,  kind: oarg.2, type: ArrInt10}
  - {name: COEF,  kind: var,  type: ArrInt5, value: "{0,1,3,2,1}"}
  - {name: _it,   kind: var, type: UInt}
  - {name: _range, kind: var, type: UInt}
  parameter:
  - {name: iterate_over, value: [in1]}
  # (...)
```

From this we see that it describes the component `mav` in our previous input model, and it instantiates a `Skeleton.ShiftFarm` template, by passing its respective ports (e.g. input/output function arguments and internal variables) and parameters (e.g. `iterate_over`, whose value is a list with one name `in1`). But what does this all mean, and where can you find more information about the template? Fortunately you do not need to dig into all the library sources. If the library designers did a good job, they should have documented each template enough for others to make good use of it. CoInSyDe (**\@0.2.2.0**) has a "build documentation" feature enabled with the `--docs` option (requires [Pandoc](https://pandoc.org/). Inkvoke it now[^4] from anywhere under the `examples` workspace:

	$ coinsyde --docs
	
This will generate a set of files in the current workspace under `docs/toy1/`, among which (**\@0.2.2.0**) `Libs.html`. Open this file now in a web browser. It contains all the raw (unaltered) information loaded into the types and components database and everything that can be inferred from it, prior to any manipulation. Search for `Skeleton.ShiftFarm`. There you can find out that:

* it is defined in `usrlib/Skeleton/coinlib-template.c.c` at line 10;
* it _requires_ a declared internal variable called `_it` and one called `_range`;
* it _requires_ a parameter called `iterate_over` which contains a list of port IDs;
* it might have other ports and parameters, but that is up to the application to define;
* it defines a placeholder called `f` with some specific requirements (we will soon get to that).

You can even see the template code, but at this point it is uninteresting. Check also the generated documentation for `mav_1`. It matches the definition in [`toy1.yaml`](../examples/proj/toy1/toy1.yaml), but now all ports are linked in a specific manner their bindings, as well as to their types. Take your time and click aroud to figure out how things are connected. When you are ready, proceed to the next section.

[^3]: Ginger is a (subset) dialect of [Jinja2](https://palletsprojects.com/p/jinja/) templating language, so users of Jinja2 will immediately recognize Ginger syntax.

[^4]: if you are really curious about which files are being parsed and loaded, you might pass the `--debug` flag as well.

#### Bindings

As hinted in the previous section, the main mechanism components interact with each other is through _reference_, which goes in the following way:

* a `template` defines a (set of) _placeholder(s)_ (or "hook(s)") where other components need to be instantiated (e.g. through function call). 
* the `pattern` defined by such a template needs to reference another component by its ID, "binding" it to a placeholder. This is done through an association list which "binds" the interfaces of the caller component to the interfaces of the callee component. 

Now we have built all the premises to understand how CoInSyDe defines and views systems: as systematic hierarchical "expansions" of templates and placeholders, where the leaf components are either native components or placeholder-less template components. This way it is capable of effectively implement higher-order mechanisms (e.g. process constructors or skeletons in ForSyDe). Functional compositions between "sibling" components however, can be achieved through means as simple as value passing (e.g. through variables in C, wires in VHDL or Verilog, or other primitive mechanisms), or as complex as through so-called "glue components" (e.g. API calls to network interfaces, custom protocol IP blocks, etc.). Whatever is the case, these decisions of "what", "where" and "when" have been long taken by a previous design flow. CoInSyDe only gets the results of such decisions as its input specification files, and its only concern is to expand the templates and generate meaningful target code. 

Let us go back to our toy example. Open again `toy.yaml` and `Libs.html` side-by-side, and see how different components are being referenced. The input YAML file contains more information, but the generated documentation presents it in a more traceable manner. Some points of interest:

* `mav_1` references `fred_1` and binds it to its placeholder called `f`. Through it it binds some of its ports to those of `fred_1`. E.g. `COEF` is bound through `(f)` to `fred_1` port called `in2`.
* `fred_1` binds its local variable `_acc` to both the `acc` and `out` ports of the referenced `mulacc` native component.
* `mav_1` binds its `out1` variable in a peculiar manner: it passes a template to a `usage` entry. For now the template is of little importance (more details are found in section [Template Language](#template-language)), but it has to do with the fact that _inside_ the `FarmReduceI` skeleton the `out1` port is seen as an _array element_, whereas _outside_, from the perspective of the `ShiftFarm` skeleton, it is seen as a _vector_ (see figure above). 

Admittedly, it is rather cumbersome to follow bindings and if they are done correctly.
Not to worry, CoInSyDe input is meant to be generated by machines, and of course "a machine never makes mistakes", does it :grin:? Until we reach that performance however, we do need to trace bindings and debug them. Fortunately, as you probably have noticed, when invoked with the `--docs` flag, CoInSyDe also has generated a DOT graph. Open/plot it now, e.g. (**@0.2.2.0**):

	$ cd path/to/examples
	$ coinsyde --docs
	$ xdot docs/toy1/Graph-main.dot

As you can see, this graph is quite useful to trace if references and bindings are done correctly and to show an overview of the design under test.

#### Generating code
 
Finally, let us experiment with the main purpose CoInSyDe was built: code generation. If you remember from the configuration file [`coinsyde.yaml`](../examples/coinsyde.yaml), the desired target platform for the `toy1` project is plain (bare-metal) C code. Open the dumped C file from the specified code path `gen/toy1/main.c`. By this time nothing in this file should be of any surprise: the generated artifact simply mirrors the specification from the input files in the [`proj/toy1`](../examples/toy1) folder, where most components are instantiated simply as functions, and called accordingly. Now compile and execute the file, e.g.:

	$ cd gen/toy1/
	$ gcc main.c
	$ ./a.out
	
No surprises there either. The artifact behaves just like the original application model: it takes 10 values and applies MAV in reverse order of their arrival. 

Notice that in [`toy1.yaml`](../examples/proj/toy1/toy1.yaml) for most of the compontents instances are specified as _not_ inline. Try changing that and see what happens:

* for the `main` function, change the instance to 
	
## Advanced Topics

TBA

### Library Management

TBA

### Input Files

TBA

### Templates Language

TBA
