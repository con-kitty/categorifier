# Categorifier GHC plugin

This package has targets for the frontend of the code generation system, using an approach that
generalizes arbitrary Haskell into a categorical representation that can then be translated to
various targets (for example, [C code](https://github.com/con-kitty/categorifier-c)).

## Usage

For this plugin to work, you need to make some changes both to targets that use it
directly, as well as ones that are depended on (transitively) by the ones that use it.

### targets you want to use `categorify` in

- enable the plugin with `-fplugin=Categorifier`,
- ensure inlining is available with `-fno-ignore-interface-pragmas` (implied by `-O` or `-O2`), and
- import `Categorifier.Categorify` to make `categorify` available (you must import the _entire_ module,
  but it may be qualified).

### targets you depend on

- use `-fno-omit-interface-pragmas` (implied by `-O` or `-O2`) to make sure inlinings are available
  and
- use `-fexpose-all-unfoldings` to avoid having to add an `inlinable` pragma to every definition
  that may need to be inlined by the plugin. **NB**: This flag adds the _optimized_ definition to
  the interface file, whereas `inlinable` adds the original definition. This distinction may be
  important during conversion.

### other steps

- define instances for your target category using your preferred type class hierarchy (the default
  is `base`)
- define `Categorifier.HasRep` instances for any types that you use in a converted function (the plugin
  will tell you if you are missing any when you try to convert)

### fine-tuning inlining sizes

It can be difficult to find a reasonable setting for the various inlining thresholds. This attempts
to lay out an approach for identifying one.

There are two significant GHC flags for adjusting inlining, `-funfolding-creation-threshold` and
`-funfolding-use-threshold`. They allow you to set an upper bound on the "size" of unfoldings that
will be considered for inlining.

1. set the `creation` (globally) threshold high, say `10000`;
2. test to see if the inlining issue goes away (if so, skip to step 5);
3. set the `use` (in `categorify` modules) threshold to match the `creation` threshold;
4. do a binary search on the `use` thresholds to minimize them as much as possible;
5. do a binary search on the `creation` thresholds to minimize them as much as possible (the lower
   bound here is probably the minimum of 750 (the default) and the `use` threshold).

If either if these values is too small, you'll end up with errors complaining that some definition
couldn't be inlined. If they're too big, you'll get errors about "simplifier ticks exhausted" (in
which case, you can bump `-fsimpl-tick-factor`) and things will take a lot longer to compile.

### defining `HasRep` instances

You should use `Categorifier.Client.deriveHasRep` for all `HasRep` instances. However, for
some GADTs you'll currently have to write out a more direct instance.

## Limitations

### Unsupported Haskell Features

The plugin attempts to convert a large part of Haskell to a category-agnostic form, however there
are some features that aren't supported (or, not supported fully).

- **`FFI`** - The plugin can not categorify arbitrary FFI calls. There is, however, support for
  parts of `libm`.
- **`IO`** - The plugin can not categorify anything that operates on the `RealWorld` state. If you
  only use `Monad` operations (etc.) on `IO`, then it should categorify fine. But then you should
  also generalize the function to an arbitrary `Monad`.
- **`LinearTypes`** - We can categorify plugins with arbitrary multiplicities (for example, linear
  functions), but we don't yet take advantage of [_Evaluating Linear Functions to Symmetric Monoidal
  Categories_](https://arxiv.org/abs/2103.06195v1) to produce simpler categorical models.
- **mutual recursion** - The plugin won’t work with any mutually-recursive definitions. Mutually-
  recursive _types_ are fine, but operations on them can't be mutually-recursive. Sometimes
  (mutually-recursive `let` or `where` bindings), the plugin can identify it and will give a
  helpful error. However, in other cases (top-level bindings) the plugin can't identify the mutual
  recursion and `categorify` will get stuck in a loop during compilation.
- **polymorphic recursion** - The plugin doesn’t currently work with polymorphically-recursive
  functions that need to be inlined (rather than directly interpreted).
- **polymorphism** - We can `categorify` polymorphic functions. However, the polymorphism may only
  be constrained by operations that we can interpret directly to the target category. For example,
  functions with types like `forall a. Foo a -> Int` or `forall a. Ord a => Foo a -> Int` can be
  categorified, but `forall a. MyCustomClass a => Foo a -> Int` may not be able to. (If
  `MyCustomClass` simply aggregates operations from type classes in `base`, then it will still
  categorify.)

### Partial Application of `Categorifier.Categorify.expression`

A call to `Categorifier.Categorify.expression` must be fully saturated to trigger the rewrite
rule installed by the plugin. For example, `expression . f $ x` or
`(if b then expression else somethingElse) (f x)` doesn’t trigger the plugin, but
`expression $ f x` and `expression (f x)` do.

If you need to support a particular kind of partial application (such as `expression . foo $ bar`),
you may want to install an extra `CoreToDo`, which runs before the plugin and performs the
necessary rewriting (for example, rewrite `expression . f $ x` into `expression (f x)`).

### Limitation of BuildDictionary

`BuildDictionary.hs` is responsible for building type class dictionaries needed by the plugin to
satisfy constraints. It currently has a limitation that it can only find orphan instances in
exposed packages, not hidden packages. For example, suppose `A.hs`, `B.hs` and `C.hs` are in
target `a`, `b` and `c`, respectively. `A.hs` imports `B.hs` and `B.hs` imports `C.hs`, but `A.hs` doesn’t
directly import `C.hs`, and target `a` doesn’t directly depend on target `c`. Then, if `C.hs`
has an orphan instance, `BuildDictionary.hs` won't see it when compiling `A.hs`, because
`C.hs` is in a hidden package `c`. The workaround is to make target `a` directly depend
on target `c`, thereby exposing `c` to `a` (but you don't need to import `C.hs` in `A.hs`).

## Comparisons

### [concat](https://github.com/compiling-to-categories/concat)

Conal Elliott's original implementation of his [_Compiling to Categories_
paper](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf). It's the basis
for this work, but has a lot of complexity and shortcomings that we've tried to address here. Some
of the improvements we've made:

- require less monomorphization,
- support alternative type class hierarchies (including support for the
  [`Arrow`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Arrow.html) hierarchy in
  [`base`](https://hackage.haskell.org/package/base),
- better error handling (both in terms of catching more things at compile time and having clearer
  messages), and
- support for sum types (in the paper, but not the current implementation of concat)
- some support for recursion via
  [traced monoidal categories](https://ncatlab.org/nlab/show/traced+monoidal+category#in_cartesian_monoidal_categories)
- require fewer fiddly pragmas (for example, `inline` and `rules`) and other modifications to the client
  code,
- simpler implementation with better modularity and more comments to hopefully make understanding
  the internals more accessible.

### [overloaded](https://hackage.haskell.org/package/overloaded)

This is a more general package, but it does offer
[`Overloaded.Categories`](https://hackage.haskell.org/package/overloaded-0.2.1/docs/Overloaded-Categories.html),
which is like Compiling to Categories applied to [the `Arrow` language
extension](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#arrow-notation).
This is ostensibly a more correct approach, given the way GHC is structured, but it relies on
`Arrow` (the extension, not the type class), and so the result is much less idiomatic Haskell.

## Contributing

### Organization

There are a bunch of modules, this calls out the most important ones when diving in.

- [`Categorifier`](./Categorifier.hs) - this is the entrypoint of the plugin, everything that hooks into
  GHC starts from here;
- [`Categorifier.Core.Categorify`](./Categorifier/Core/Categorify.hs) - the high-level logic of the
  categorical transformation as described in Conal's paper, it tries to define as clearly as
  possible the mapping from **Hask** to abstract categories;
- [`Categorifier.Hierarchy`](./Categorifier/Hierarchy.hs) - the mappings from abstract
  categories to specific type class hierarchies.
- [`Categorifier.Test.Hask`](../plugin-test/Categorifier/Test/Hask.hs),
  [`Categorifier.Test.Term`](../plugin-test/Categorifier/Test/Term.hs),
  [`Categorifier.Test.TotOrd`](../integrations/concat/integration-test/Categorifier/Test/TotOrd.hs) - various
  categories defined (often against many type class hierarchies) for testing the plugin.

### Debugging

Writing a plugin is _mostly_ the same as vanilla Haskell. The primary distinctions are

1. you have an entrypoint other than `main` and
2. you have to use the GHC API.

The first means that there is a lot of stuff happening outside your code, and your code may be
called zero or many times, the expressions that your code is applied to may be different than
what you were expecting, or than what you created in a previous step, etc.

The second means dealing with decades of evolutionary code. We try our best to code defensively
(for example, call `foo_maybe` instead of `foo`), but things like `panic`s and other surprising
behavior still surface.

As we work through the implementation, the types of debugging we use may change, so this section is
expected to churn a bit, as new approaches are added and old ones are obsolesced.

#### dealing with failed tests

We use a flag, `Categorifier:defer-failures`, to keep conversion failures from crashing GHC. This is
useful in tests so that we can collect all failures, rather than exiting on the first one. But in
non-testing situations, we _want_ compilation to fail.

#### catching missed identifier conversions

The last case of `findMaker` tries to inline the identifier, which can be useful to track but it's
possible for this to cause a cycle with `categorifyLambda (Plugins.App ...)`. To discover if you're
running into this, replace the `Nothing` with `error [fmt|missing: {modu}.{var} {length args}|]` and
see if you're trying to match the correct application.

#### analyzing Core-to-Core transformations

We compile all the hierarchy tests with `-dcore-lint`, which catches cases where we’re generating
at least slightly off (and possibly bogus) Core.

The Core output (from either `-dcore-lint` or our own tracing) can be pretty noisy, so
`-dsuppress-all` substantially shortens the output, but it can also hide some critical
information. You can control the suppression a bit more with options like `-dsuppress-coercions` and
`-dsuppress-type-applications` (and their `-dno-` inverses).

If you need to inspect Core when there isn't a linting issue, you can add `-dverbose-core2core` to
see _every_ transformation that happens.

#### getting information from a hang

Sometimes the plugin doesn't terminate (hopefully we can completely eliminate this possibility by
treating conversion as an inductive fold in future). When this happens, there are a few ways you can
recover what's happening.

If your GHC was built with `libdw` support (ours currently isn't) then you can send
[`SIGQUIT`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/debug-info.html#requesting-a-stack-trace-with-sigquit)
(or `Ctrl-\`) to the process to get a stack trace of where it exited.

Or, sending `SIGUSR2` will cause any accumulated `IO` actions to run before the program
exits. This can be useful for recovering any tracing output.
