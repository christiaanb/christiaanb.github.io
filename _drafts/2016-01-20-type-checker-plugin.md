---
layout: post
title:  "GHC Type checker plugins: adding new type-level operations"
date:   2016-01-20 09:10:00
categories: draft
tags: haskell dependent_types type_checker_plugins
comments: false
analytics: false
---

Since version 7.10.1, GHC supports so-called [type-checker plugins](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/compiler-plugins.html#typechecker-plugins) which let us extend GHC's constraint solver, i.e. extend the the range of programs GHC can type check.
Several plugins have already been have already been released, the ones I know are:

* Adam Gundry's [uom-plugin](http://hackage.haskell.org/package/uom-plugin) for supporting units of measure.
* Iavor Diatchki's [type-nat-solver](https://github.com/yav/type-nat-solver) which allows you to hook up SMT solvers to solve numeric constraints.
* My own plugins:
  * [ghc-typelits-natnormalise](http://hackage.haskell.org/package/ghc-typelits-natnormalise) which solves numeric constraints using a custom normalisation procedure as opposed to Iavor's approach which uses SMT solvers.
  * [ghc-typelits-extra](http://hackage.haskell.org/package/ghc-typelits-natnormalise) which adds a type-level greatest common denominator (GCD) and (ceiling of) logarithm operator for GHCs type-level `Nat`ural numbers.

This post will be about writing our own type-checker plugin: we will build a plugin that (just like [ghc-typelits-extra](http://hackage.haskell.org/package/ghc-typelits-natnormalise)) allows GHC to perform GCD on type of kind `Nat`.
<br>
# Why a type-checker plugin?
So the first question that might pop up: why do we even need to write a type-checker plugin to support GCD over `Nat`? The "problem" is that `Nat` is not defined inductively like so:

{% highlight haskell %}
data Nat = Z | S Nat
{% endhighlight %}

, however, it is simply defined as an `Integer` (see [CoreSyn.hs](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/src/TypeRep.html#TyLit)).

For an inductively defined `Nat` we could simply write a (closed) type family symbolising our (arithmetic) operations, e.g.:

{%highlight haskell %}
{-# LANGUAGE DataKinds, TypeFamilies #-}

import Data.Proxy

data Nat = Z | S Nat

type family Add (x :: Nat) (y :: Nat) :: Nat where
  Add Z b = b
  Add (S a) b = S (Add a b)

test :: Proxy (S (S (S (S Z)))) -> Proxy (Add (S (S Z)) (S (S Z)))
test = id
{% endhighlight %}

But, inductively defined `Nat`s is not what we have, we have `Integer`s, we might start with:

{%highlight haskell %}
{-# LANGUAGE DataKinds, TypeFamilies #-}

import GHC.TypeLits

type family Add (x :: Nat) (y :: Nat) :: Nat where
  Add 0 b = b
  Add 1 b = ???
{% endhighlight %}

where we have no real way to define the part indicated by "???".
Indeed, all the currently defined type-level operations on types of kind `Nat` as defined in [GHC.TypeLits](http://hackage.haskell.org/package/base-4.8.1.0/docs/GHC-TypeLits.html#g:3), are all implemented by compiler magic.
Using type-checker plugins, that kind of magic is, however, no longer just reserved for the wizards over at GHC HQ!

As to why GHCs native type-level natural numbers are implemented using `Integer`, instead of an inductively defined datatype, I can only guess:

* Unary encodings (such as the one seen earlier) are "expensive", in terms of memory, for large numbers.
I'm sure there must be a way to internally use a more efficient representation for what is, from the user's point of view, a unary encoding.
To my my knowledge, no efforts have been made to explore this option in GHC.
* Base-2 encodings, though low-cost in terms of memory, have harder to define arithmetic operations.

With the advent of type-checker plugins, the `Integer`-representation aspect of `Nat` has not been a problem any more for [my projects](http://clash-lang.org).

# Type-level GCD

OK, enough about why we need to implement GCD using a type-checker plugin.
Let's get on with implementing it.
Before we implement the plugin, we first need to define our GCD operation:

{%highlight haskell %}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module GHC.TypeLits.GCD
  ( GCD )
where

import GHC.TypeLits (Nat)

type family GCD (x :: Nat) (y :: Nat) :: Nat where
  GCD 0 x = x
{% endhighlight %}

The reason we've chosen a closed type-family (as opposed to an open one) is that we don't want our users to pull any shenanigans such as adding additional bogus equations.
For GHC 7.10 we need to add at least one equation (see GHC issue [#9840](https://ghc.haskell.org/trac/ghc/ticket/9840)) to our closed type family; this should no longer be needed for GHC 8+.
Although an empty closed type family is possible in GHC 8+, the current single equation version simplifies the design of the type-checker plugin, as we will see later.

# Type-checker plugin

I guess the reason there are not too many blog posts on GHC type checker plugins is because you need to be sort-of familiar with the GHC API.
I've been using the GHC API for over 6 years now, so I've taught myself how to browse its documentation (the comments in the source code); however, it's lack of haddock documentation makes it somewhat intimidating for the uninitiated.
I try to keep the GHC API code to a minimum, but it can sadly not be avoided.

We first start with some imports:

{%highlight haskell %}
{-# LANGUAGE CPP, TupleSections #-}
module GHC.TypeLits.GCD.Solver
  ( plugin )
where

-- external
import Control.Arrow       ((***))
import Data.List           (partition)
import Data.Maybe          (mapMaybe)
import GHC.TcPluginM.Extra (evByFiat, lookupModule, lookupName)

-- GHC API
import FastString (fsLit)
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm)
import TcPluginM  (TcPluginM, tcLookupTyCon)
import TcRnTypes  (Ct, TcPlugin(..), TcPluginResult (..), ctEvidence,
                   ctEvPred)
import TcType      (typeKind)
import TyCon      (TyCon)
import Type       (EqRel (NomEq), Kind, PredTree (EqPred),
                   classifyPredType, eqType)
#if __GLASGOW_HASKELL__ >= 711
import TyCoRep    (Type (..), TyLit (..))
#else
import TypeRep    (Type (..), TyLit (..))
#endif
import TysWiredIn (typeNatKind)
{% endhighlight %}

Aside from the GHC API, and some Prelude modules, we also import some functions from my [ghc-tcplugins-extra](http://hackage.haskell.org/package/ghc-tcplugins-extra) package, `GHC.TcPluginM.Extra`: it provides some type-checker plugin utility functions with the intention to provide a stable API covering multiple GHC releases.
Currently [ghc-tcplugins-extra](http://hackage.haskell.org/package/ghc-tcplugins-extra) supports GHC 7.10 and GHC 8.0.

Next up, setting up the plugin:

{%highlight haskell %}
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const (Just gcdPlugin) }

gcdPlugin :: TcPlugin
gcdPlugin =
  TcPlugin { tcPluginInit  = lookupGCDTyCon
           , tcPluginSolve = solveGCD
           , tcPluginStop  = const (return ())
           }
{% endhighlight %}

We use `defaultPlugin` to create a plugin that does nothing at all, and add our own `gcdPlugin` type-checker plugin.
Plugin's can be given [command line options](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/flag-reference.html#idp15444912), but we just ignore them.

The `TcPlugin` record has three fields:

* `tcPluginInit`: Initialise the state used by our type-checker plugin.
  We will use it to look up the `TyCon` (type constructor) of the `GCD` type family.
  One can also perform IO operations, and have the state contain `IORef`s.
* `tcPluginSolve`: The main solver function, this is were we will actually perform the _gcd_ calculation.
* `tcPluginStop`: Cleaning up of the state.
  We have no use for it, but it can for example be used to remove files that were used as a communication medium with an SMT solver.

Our type-checker plugin solves equations involving our `GCD` type family, so we need to know that the type that we are dealing with actually involves `GCD`.
To do that, we need GHC's internal representation of `GCD`, which is a [TyCon](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/TyCon.html#t:TyCon).
We do the lookup of our `GCD` TyCon in the initialisation of our type-checker plugin, so that it becomes of our plugin's internal state:

{%highlight haskell %}
lookupGCDTyCon :: TcPluginM TyCon
lookupGCDTyCon = do
    md      <- lookupModule gcdModule gcdPackage
    gcdTcNm <- lookupName md (mkTcOcc "GCD")
    tcLookupTyCon gcdTcNm
  where
    gcdModule  = mkModuleName "GHC.TypeLits.GCD"
    gcdPackage = fsLit "ghc-typelits-gcd"
{% endhighlight %}

The name of our package is going to be "ghc-typelits-gcd", and the module in which we defined `GCD` is "GHC.TypeLits.GCD".
In the above code, we first look up the `GHC.TypeLits.GCD` module in the `ghc-typelits-gcd` package, we then look up the full `Name` of the thing we call "GCD", and finally look up the actual `GCD` TyCon.

{%highlight haskell %}
solveGCD :: TyCon
         -> [Ct] -- ^ [G]iven constraints
         -> [Ct] -- ^ [D]erived constraints
         -> [Ct] -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solveGCD _     _ _ []      = return (TcPluginOk [] [])
solveGCD gcdTc _ _ wanteds = return $! case failed of
    [] -> TcPluginOk (mapMaybe (\c -> (,c) <$> evMagic c) solved) []
    f  -> TcPluginContradiction f
  where
    gcdWanteds      = mapMaybe (toGCDEquality gcdTc) wanteds
    (solved,failed) = (map fst *** map fst)
                    $ partition (uncurry (==) . snd) gcdWanteds

reduceGCD :: TyCon -> Type -> Maybe Integer
reduceGCD gcdTc = go
  where
    go (LitTy (NumTyLit i)) = Just i
    go (TyConApp tc [x,y])
      | tc == gcdTc = gcd <$> go x <*> go y
    go _ = Nothing

toGCDEquality :: TyCon -> Ct -> Maybe (Ct,(Integer,Integer))
toGCDEquality gcdTc ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2
      | isNatKind (typeKind t1) || isNatKind (typeKind t1)
      -> (ct,) <$> ((,) <$> reduceGCD gcdTc t1 <*> reduceGCD gcdTc t2)
    _ -> Nothing
  where
    isNatKind :: Kind -> Bool
    isNatKind = eqType typeNatKind

evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "ghc-typelits-gcd" t1 t2)
    _                  -> Nothing
{% endhighlight %}
