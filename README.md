# An Interpreted, Object-oriented Language, Based On A Register-oriented Virtual Machine

## Goals

- Minimalism
- Uniformity
  - Everything is a first-class object
  - Uniform method names
- Functional programming

## Influences

- Smalltalk: method invocation style
- Lisp: program-data equivalence, lists, car and cdr, NIL
- C++: comments
- FORTH: prompt

## Architecture

- Lightweight virtual machine

## Grammar

### Expression

An expression is one of: an atom, a pair, a list, a method call, or a block.

### Atom

An atom is one of: a number, a boolean, #nil, or a string.

Examples: 13, 0x13, -45.67, #true, sam, "This is a string"

The evaluated value of an atom is itself, with the the exception of strings, which evaluate to the value bound to the string in the environment.

### Pair

A pair is a left paren ('('), an expression, a comma (','), an expression, and a right paren (')').

Examples: (1, 2), (#true, "foo")

The evaluated value of a pair is a pair of the evaluated values of each of its constituents.

### List

A list is a left paren ('('), followed by 0 or more expressions, separated by spaces, and a right paren (')').

Examples: (this is a list), (42 3.1425 #true "sam")

The evaluated value if a list is a list of the evaluated values of each of its constituents.

### Method call

A method call is a left square bracket ('['), followed by interleaved expressions and selectors, separated by spaces, followed by a right square bracket (']').

The evaluated value of a method call is the result of calling the given method with the given arguments.  The method name is the concatenation of all selectors, and the method receiver is the first argument.  If the receiver is a class, the receiver's class-methods dictionary is consulted; otherwise, the instance-method dictionary for the class of which the receiver is an instance is consulted.  If no method is found, the parent class' dictionary is consulted, all the way up to the root class (#Object).  If the method name begins with an ampersand ('&'), evaluataion of the method arguments is suppressed.  If the method name begins with 2 ampersands ("&&"), the result of the method call is evaluated.

Examples: [1 add: 2], [#Metaclass new: "Foo" parent: #Object instance-variables: '(a b c)]

### Block

A block is a left brace ('{'), followed by a list, which are the arguments to the block, followed by a sequence of expressions, separated by spaces, followed by a right brace ('}').

The evaluated value of a block is either the result of executing the body expressions, if the block has no arguments; otherwise, the block itself, if the block has arguments.  A block with arguments can be evaluated using the eval: method, where a single list argument to eval: is the list of arguments passed to the block.

Examples: {() [i print] [i = [i add: 1]] }, {(a b) [a add: b]}

### Short forms

'x        is short for [x &quote]

[x := y]  is short for [#Environment def: 'x put: y]

[x = y]   is short for [#Environment at: 'x put: y]

x.y       is short for [x at: 'y]

[x.y = z] is short for [x at: 'y put: z]

For more examples, see the module "point.ool".

## Todo

Bugs

Features

Enhancements
- Blocks with no arguments; evalled as a sequence of exprs by "eval"
- (1) More efficient representation for method calls
- Use readline for interactive input
- Perhaps generalization of dotted-string?  @xxx.yyy.zzz, where '@' introduces string,
  white space terminates it, and subunits (which can be arbitrary characters, i.e. arbitrary expressions)
  are separated by periods?  Or xxx@yyy@zzz?
- Scoped instance vars and methods; private, protected, public
- Read only object attributes vs. acessor methods?
- Exceptions?

Gotchas?
- break can operate across block boundaries

Optimizations
- Make m_xxx functions return a value, too
- Better internal representations
  - Blocks: separate args and body
  - (1) More efficient representation for method calls
- Memory management
  - Keep freed blocks in lists ordered by size, for re-use
  - Instead of freeing instances (objects), keep a list of free instances
    linked off the class; garbage collection frees them first
    - How deal with objs with additional storage, eg. strings, arrays?
- Simplify scanner
- Inline functions

Etc.
- Finalize socket, math, process modules
- Additional modules