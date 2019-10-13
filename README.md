# CodaLang, a domain specific language for [CodaLab](https://worksheets.codalab.org/) <!-- omit in toc -->

CodaLang is a DSL for workflow management on CodaLab. Originally designed for [Workflow](https://github.com/jyh1/workflow), it is a high level functional language where the basic value is CodaLab `bundle` and can be compiled down to CodaLab [CLI](https://codalab-worksheets.readthedocs.io/en/latest/CLI-Reference/).

## Table of contents
- [Table of contents](#table-of-contents)
- [Getting started](#getting-started)
- [Tutorial](#tutorial)
  - [Bundle as a value](#bundle-as-a-value)
  - [Run Expression](#run-expression)
  - [Let Expression](#let-expression)
  - [Defining a Pipeline](#defining-a-pipeline)
  - [Function](#function)
  - [Options for cl run](#options-for-cl-run)
  - [String Expression](#string-expression)
  - [Record](#record)
  - [Types](#types)
- [Build](#build)

## Getting started

The easiest way to experiment with CodaLang is through the [demo web app](http://13.82.168.247/demo/codalang/). The demo app is able to emit shell script based on the `cl` command from your input expression. For actually using CodaLang, check out the [Workflow](https://github.com/jyh1/workflow), a workflow manager with a visual programming interface built on top of CodaLab.

## Tutorial

> **_Tip:_** You can copy/paste the example programs from this tutorial to the [demo web app](http://13.82.168.247/demo/codalang/) to see the actual compiled results.

> **_Note:_** This tutorial assumes you already have a working knowledge about CodaLab. If not, a quick start guide can be found [here](https://github.com/codalab/worksheets-examples).


### Bundle as a value
CodaLang is all about the `bundle`. In other programming language, you probably can write something like `32` to create a constant integer. In CodaLang, you can literally write out a bundle UUID to create a constant bundle expression:

```
0x3b5f831f09f04a22bcd3020b7a1cb69c
```

It refers to this [bundle](https://worksheets.codalab.org/bundles/0x3b5f831f09f04a22bcd3020b7a1cb69c) in the public CodaLab server. The above example is a bona fide CodaLang expression, although a rather boring one. It will get compiled, but no command will be generated, because it is just a constant expression.

### Run Expression
You can do something more interesting with a `run expression`. The Hello World program for CodaLang might look like:
```
@echo Hello World@
```

A `run expression` is created in CodaLang
by simply enclosing the shell command with a pair of `@`s.
Compile it on the [demo app](http://13.82.168.247/demo/codalang/), you should get 
something like:

```shell
bundle_0=$(cl run  --name codalang-1   'echo Hello World')
```

It contains a single `cl run` command which just submits our `echo Hollo World` to CodaLab.

A `run expression` is also a bundle value. During runtime, it will be evaluated to the
UUID returned by `cl run`.

The syntax of `run expression` actually behaves like a string template, you can directly embed CodaLang
expressions in it by using a `${EXPR}` syntax, e.g.:
```
@cat ${0x3b5f831f09f04a22bcd3020b7a1cb69c}@
```

Here we embed a UUID, i.e. a bundle expression, in the `run expression`. 
But what does it mean? Let's take a look at the generated command:

```shell
bundle_0=$(cl run  --name codalang-2  codalang-1:0x3b5f831f09f04a22bcd3020b7a1cb69c 'cat codalang-1')
```

What happened is that CodaLang replaces the embedded bundle value with a name (`codalang-1`) and adds a dependency declaration to the `cl run` command. 
Inside CodaLab, the command can actually access the content of that bundle 
with the file path `codalang-1`.

You can embed arbitrary expression in this way. For example:
```
@cat ${@echo Hello World@/stdout}@
```

Basically a `run expression` is embedded inside another one. 
We finally create a simple pipeline in CodaLang! 
The outermost `cat` command will rely on the output of the inner `echo` command.

Generated commands:
```shell
bundle_0=$(cl run  --name codalang-1   'echo Hello World')
bundle_1=$(cl run  --name codalang-3  codalang-2:$bundle_0/stdout 'cat codalang-2')
```

Note the command uses the `bundle_0` shell variable in the dependency declaration in the second command.

However, this syntax will quickly become unmanageable for pipelines with many intermediate steps.
For this reason, we normally use a `let` expression.

### Let Expression

A simple `let` expression looks like:

```haskell
let 
    a = 0x3b5f831f09f04a22bcd3020b7a1cb69c
in
    a
```

The `let` expression allows us to introduce local name bindings and use them inside the result expression after `in`.
The above boring expression will be evaluated to the bundle `0x3b5f831f09f04a22bcd3020b7a1cb69c`.

A more involved example:

```haskell
let 
    a = 0x3b5f831f09f04a22bcd3020b7a1cb69c;
    sort = 0xd4c5712c156f41e48e6400f05cc5441c;
in
    @ python $sort < $a @
```

Multiple bindings in `let` are separated by `;`.
Here we are binding two bundle values to names `a` and `sort` 
and then using them inside the final `run` expression.


Because we are embedding two bundle values in the `run` expression, we should find two dependencies in the generated command:

```shell
cl run  --name codalang-3  a-1:0x3b5f831f09f04a22bcd3020b7a1cb69c sort-2:0xd4c5712c156f41e48e6400f05cc5441c ' python sort-2 < a-1 '
```

Using a name binding also help the compiler pick names when generating bundle dependencies. 
In this example, the compiler chose `sort-2` and `a-1` instead of `codalang-*` we saw before.
We will see later how to absolute control the names in generated bundle dependencies.

### Defining a Pipeline
Our original motivation for using a `let` expression is to make it easier to define long pipelines. Here it is:
```haskell
let 
    a = 0x3b5f831f09f04a22bcd3020b7a1cb69c;
    sort = 0xd4c5712c156f41e48e6400f05cc5441c;
    a2 = @cat $a $a@/stdout;
    a4 = @cat $a2 $a2@
in
    @ python $sort < $a4/stdout @
```

Basically, we assign names to several `run` expressions and use them to construct new ones, therefore forming a pipeline.

The `/` is an operator in CodaLang. We can use it to specify a sub-folder or file of a bundle.
We use it when defining `a2` in the above example: `@cat $a $a@/stdout`. 
We can also do something like `0xf2b97928e58c47c7a45c1ad95f665410/stdout`. 
In general, the syntax `<bundle>/path/path/.../path` is just another form of bundle expression in CodaLang. It will be 
evaluated to a bundle value, although technically it doesn't have a UUID directly associated with it in CodaLab. 


Take a look at the following examples to see how the dependencies are generated based on the value embedded in `run`:

|  Expression |  Generated Dependency  |
|:------------------:|:-----------:|
| @cat ${0xf321}/stdout@ | a:0xf321 |
| @cat ${0xf321/stdout}@ | a:0xf321/stdout |
| let a = 0xf321 in @cat ${a}/stdout@ | a:0xf321 |
| let a = 0xf321 in @cat ${a/stdout}@ | a:0xf321/stdout |

(When embedding a variable in the `run` expression,  `$a` is a shorthand for `${a}`).

The compiled results of our pipeline example above:
```shell
bundle_0=$(cl run  --name a2-3  a-1:0x3b5f831f09f04a22bcd3020b7a1cb69c 'cat a-1 a-1')
bundle_1=$(cl run  --name a4-5  a2-4:$bundle_0/stdout 'cat a2-4 a2-4')
bundle_2=$(cl run  --name codalang-6  a4-5:$bundle_1 sort-2:0xd4c5712c156f41e48e6400f05cc5441c ' python sort-2 < a4-5/stdout ')
```
 
### Function
If you still haven't been sold on CodaLang, behold the "killer app" --- function definition. It was supposed to be a functional programming language after all!

```haskell
let 
    append[a: bundle, b: bundle] = @cat $a $b@/stdout;
    double[file: bundle] = append[a: file, b: file];
    quadruple = [a: bundle] => let a2 = double[file: a] in double[file: a2];
in
    quadruple[a: 0x3b5f831f09f04a22bcd3020b7a1cb69c]
```
We defined three functions here, `append`, `double` and `quadruple`. The syntaxes are slightly different, the form in the first two definitions is just a syntax sugar for the lambda form used in `quadruple`. Take the definition of `append` as example, we first declare two arguments (`a` and `b`) along with their types (both are `bundle` inside a pair of square brackets, we will talk about type later). The body of the function is just a normal CodaLang expression with the declared argument variables in its scope. We can also write it in the lambda form: `[name: type ...] => expr`.

In the caller's side, we need to provide a dictionary-like structure or so called "keyword arguments". The syntax is similar to the definition side, pairs of colon-separated name and expression: `function[name: expr ...]`.

The compiled script of this example:
```shell
bundle_0=$(cl run  --name a2-3  a2-1:0x3b5f831f09f04a22bcd3020b7a1cb69c 'cat a2-1 a2-1')
bundle_1=$(cl run  --name codalang-5  a2-4:$bundle_0/stdout 'cat a2-4 a2-4')
```

A few extra notes: CodaLang is designed to support `Workflow`(https://github.com/jyh1/workflow). The `function` in CodaLang is closely modelled as a `tool` in Workflow, with named input ports and output ports.
![node](img/node.png)

Function in CodaLang is currently more like macro. All functions will be eliminated after compilation. However, it do behave like a value in the sense that it can be passed around as an argument or as a return value from another function with correctly implemented closure. It has a function type which will be discussed in the [type](#types) section. 

### Options for cl run
We can specify `cl run` options in the CodaLang `run` expression.
```
@ --request-docker-image codalab/default --request-gpus 2 #echo Hello World@
```
Option specifications and the actual command are separated by `#`. The above example will be compiled to:
```shell
bundle_0=$(cl run  --name codalang-1  --request-docker-image codalab/default --request-gpus 2   'echo Hello World')
```

### String Expression
String expression is similar to a regular string expression in other language. It is defined with a pair of double quote with common quotation rules.

```haskell
let lr = "1e-5" in @ echo $lr@
```
Compiled to:
```shell
bundle_0=$(cl run  --name codalang-1   ' echo 1e-5')
```

String expression will be inlined directly when embedded in a `run` expression. 
Special shell characters like `$`, white space and etc will be quoted. String expression has type `string`, which could be passed as argument.
```
let 
    gpuNum = "3";
    max = "5";   
in
    @ --request-gpus=$gpuNum # echo $max @
```

Compiled result:
```
bundle_0=$(cl run  --name codalang-1  --request-gpus=3   ' echo 5 ')
```

### Record
A record expression looks like:
```haskell
let a = 0x3b5f831f09f04a22bcd3020b7a1cb69c
in 
    {data1: 0xd4c5712c156f41e48e6400f05cc5441c
    , data2: a
    }
```
Record is just a series of key-value pairs enclosed by `{}`. It is mainly used for defining function with multiple results. We can lookup its content with the `/` operator. For example, `{k1: v1, k2: v2 ...}/k1` will be evaluated to `v1`.

```haskell
let 
    multiply[a: bundle] = 
        {
              double: @cat $a $a@/stdout
            , triple: @cat $a $a $a@/stdout
        };
    sextuple[file: bundle] = 
        let doubled = multiply[a: file]/double
        in multiply[a: doubled]/triple;
    data = 0x3b5f831f09f04a22bcd3020b7a1cb69c
in
    sextuple[file: data]
```
In this example, function `multiply` returns a record with two values: `double` and `triple`. The `sextuple` function is built by first picking the `double` key and then the `triple` key.

CodaLang acts like a "lazy" language, which means only the necessary commands that are required to compute the final result will be generated. For example, even though `multiply` is called twice, only two commands that are necessary for the final result are generated:

```
bundle_0=$(cl run  --name doubled-2  data-1:0x3b5f831f09f04a22bcd3020b7a1cb69c 'cat data-1 data-1')
bundle_1=$(cl run  --name codalang-7  doubled-4:$bundle_0/stdout 'cat doubled-4 doubled-4 doubled-4')
```

### Types
The values embedded in a `run` expression will be treated differently depending on whether it is a string or bundle. 
If it is a bundle, we need to replace it with a name and then generate correct dependency declaration.
If it is a string, we just need to inline it in the command.
It is necessary for CodaLang to keep track of the type to behave correctly.

Types of some example expressions:

|  Expression |  Type  |
|:------------------:|:-----------:|
| "a b c d" | string |
| 0x3b5f8 | bundle |
| 0x3b5f8/stdout | bundle |
| let a="a b c" in a | string |
|{a: "aa", b: 0x3b5f8} | {a: string, b: bundle} |
| [a: bundle] => @cat $a@ | [a: bundle] => bundle|



## Build
This package is intended to be used as a library, but it comes with a minimal CLI for interpreting CodaLang file through the system `cl` command. It can be built with [stack](https://tech.fpcomplete.com/haskell/get-started)
```bash
stack build && stack test
```
