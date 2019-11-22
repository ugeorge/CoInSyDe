# coinsyde

## User guide

This guide is temporary and is subject to change. Better watch the API for the latest changes.

### Notation

The tool recognizes `XML`, `JSON` and `YAML` files. Any input file needs to carry the same information, but in the format specific to its markup language. Check any tutorial on moving from one format to another, e.g. [this one](https://www.csestack.org/yaml-vs-json-vs-xml-difference/). Notable differences: 

1. YAML and JSON use generic records instead of attributes in XML
2. XML can only pass string information, whereas the other two are typed. CoInSyDe tries to make up for this as much as possible, however limitations might occur.
3. in the guides below `CTEXT` denotes code text. XML supports text information inside a node, but YAML and JSON don't. For the latter we store `CTEXT` inside a record called `code`.

In the notations below we use the following convention 

* `*` means 0 or more child nodes of this type; `+` means 1 or more;
* `@!` means mandatory attribute, `@?` means optional attribute.
* `x|y` means either `x` or `y`


## Common Elements

	root
	* pattern(...)
	* template(...)
	* native(...)
	* type(...)

Pattern is a component that has no template of its own, but "steals" the template from another (existing, ideally library) component.

	pattern{@!name:<own_id>, @!type:<existing_id>}
	* interface(...)
	* instance(...)
	* requirement(...)

Template is a component that needs to define the template code. Ideally the user does not define one herself, but loads one from a library.

	template{@!name:<own_id>, !CTEXT}
	* interface(...)
	* instance(...)
	* requirement(...)

Native is a component which contains native target code instead of a template. It is _not allowed_ to be called inline, only as function call. Might have either the code as `CTEXT` or point to a file where it is already defined through an `include`-like requirement.

	native{@!name:<own_id>, ?CTEXT}
	* interface(...)
	* instance(...)
	* requirement(...)

An instance is a binding of a `placeholder` inside the template to another component.

	instance{@!placeholder:<name>, @!component:<callee_id>}
	* bind{@!replace:<callee_if_name>, @![with:<own_if_name>|withValue:<value>]}

The binding `withValue` is the equivalent of creating and binding a parameter-like interface in the callee called `__internal__`. 

### C-Specific Format

__OBS:__ top modules are components called `top_<name>`, and the name will be given to the generated C file.

Check out API documentaion of the [`CoInSyDe.Backend.C.Core`](src/CoInSyDe/Backend/C/Core.hs) module for the supported target types and how to define them.

For now requirements are specified only as include dependencies.

	requirement{@!include:<file>}

A combination between interface type and its class determines the syntax for declaring, instantiating and using the program variables. There are two types of interfaces: variables and parameters.

	interface{@!name:<if_id>, @!class="param", @!value=<value>}
	interface{@!name:<if_id>, @!class=<var_class>, @!type=<type_id>, @?value=<value>, @?constructor=<component_id>}
	
where `<var_class>` may be:

* `iarg`, `oarg` for input/output function arguments. Outputs are defined as pointer arguments.
* `ret` is _the_ return variable for a function (allowed only one)
