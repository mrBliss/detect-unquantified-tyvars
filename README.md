Detect Unquantified Type Variables
==================================

This is a GHC source plugin to detect type variables that are unquantified.


What is an unquantified type variable?
--------------------------------------

A type variable that has not been brought into scope by an explicit `forall`.
For example:

```haskell
identity :: forall t. t -> t
identity x = x

const :: a -> b -> a
const x _ = x
```

The type variable `t` in the type signature of `identity` is a quantified type
variable, whereas `a` and `b` are unquantified type variables.

After activating this source plugin by adding the following line to your file:
```
{-# OPTIONS -fplugin=DetectUnquantifiedTyVars #-}
```

You will get the following warnings:

```
warning: Type variable without explicit forall: a
  |
  | const :: a -> b -> a
  |          ^

warning: Type variable without explicit forall: b
  |
  | const :: a -> b -> a
  |               ^
```


Why would you want this?
------------------------

Maybe if you want to enforce a more explicit code style where programmers have
to quantify each type variable with a `forall`, which can be useful when
you're using `-XTypeApplications`. Maybe you have written some PureScript,
where `forall`s are mandatory, and you have become a fan of this style. **This
was just an excuse to play around with GHC's new source plugin mechanism.**


How can I try this out?
-----------------------

Source plugins are a feature that will be included in GHC 8.6. At the time of
writing, this version has not yet been released. So you will have to compile
[GHC from source][QuickStart], or on Ubuntu, you can use hvr's [PPA]. Next,
have a look at the `test` folder.

For example, using cabal new-build, you can add the following line to
`cabal.project`:

```
with-compiler: /path/to/ghc/inplace/bin/ghc-stage2
```

You can compile by running the following command:

```
cabal new-build test
```

Note that you may get the following warning when using this source plugin:

```
WARNING in hptSomeThingsBelowUs
  missing module DetectUnquantifiedTyVars
  Probable cause: out-of-date interface files
```

This is a GHC bug, see [#15234]. However, everything seems to work fine, so
you can ignore it.


Lessons learned
---------------

Writing a source plugin turned out to be quite simple, however getting the
detection of unquantified type variables right turned out to be quite complex.
Most of the complexity comes from the fact that the type variables of a type
signature are also scoped of the pattern and expression signatures in its
body, combined with the signatures of local bindings, which can nest
arbitrarily deep.


[QuickStart]: https://ghc.haskell.org/trac/ghc/wiki/Building/QuickStart
[PPA]: https://launchpad.net/~hvr/+archive/ubuntu/ghc
[#15234]: https://ghc.haskell.org/trac/ghc/ticket/15234
[syb]: http://hackage.haskell.org/package/syb
