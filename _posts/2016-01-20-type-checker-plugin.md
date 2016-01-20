---
layout: post
title:  "GHC type checker plugins: adding new type-level operations"
date:   2016-01-20 09:10:00
categories: blogging
tags: haskell dependent_types type_checker_plugins
comments: true
analytics: true
---

Since version 7.10.1, GHC supports so-called [type-checker plugins](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/compiler-plugins.html#typechecker-plugins) which let us extend GHC's constraint solver, i.e. extend the the range of programs GHC can type check.
Several plugins have already been released, the ones I know are:

* Adam Gundry's [uom-plugin](http://hackage.haskell.org/package/uom-plugin): for supporting units of measure.
* Iavor Diatchki's [type-nat-solver](https://github.com/yav/type-nat-solver): which allows you to hook up SMT solvers to solve numeric constraints.
* My own plugins:
  * [ghc-typelits-natnormalise](http://hackage.haskell.org/package/ghc-typelits-natnormalise): which solves numeric constraints using a custom normalisation procedure as opposed to Iavor's approach, which uses SMT solvers.
  * [ghc-typelits-extra](http://hackage.haskell.org/package/ghc-typelits-natnormalise): which adds a type-level greatest common denominator (GCD) and (ceiling of) logarithm operator for GHCs type-level `Nat`ural numbers.

This post will be about writing our own type-checker plugin: we will build a plugin that (just like [ghc-typelits-extra](http://hackage.haskell.org/package/ghc-typelits-extra)) allows GHC to perform GCD on types of kind [Nat](http://hackage.haskell.org/package/base-4.8.1.0/docs/GHC-TypeLits.html#t:Nat).
<br>
# Why a type-checker plugin?
So the first question that might pop up: why do we even need to write a type-checker plugin to support GCD over `Nat`? The "problem" is that `Nat` is _not_ inductively defined like so:

{% highlight haskell %}
data Nat = Z | S Nat
{% endhighlight %}

, however, it is simply defined as an `Integer` (see [CoreSyn.hs](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/src/TypeRep.html#TyLit)).

For an inductively defined `Nat`s, it is fairly simple to write a (closed) type family symbolising an (arithmetic) operation, e.g.:

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

However, an inductively defined `Nat`s is not what GHC gives us, GHC gives us `Integer`s.
Having only `Integer`s, we might try to start with:

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

With the advent of type-checker plugins, the `Integer`-representation aspect of `Nat` has not been a problem any longer for most of [my projects](http://clash-lang.org).

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

The reason I've chosen a closed type-family (as opposed to an open one) is that I don't want our users to pull any shenanigans such as adding additional bogus equations.
For GHC 7.10 we need to add at least one equation (see GHC issue [#9840](https://ghc.haskell.org/trac/ghc/ticket/9840)) to our closed type family; this should no longer be needed for GHC 8+.
Although an empty closed type family is possible in GHC 8+, the current single equation version simplifies the design of the type-checker plugin, as we will see at the end of this blog post.

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
import TcRnTypes  (Ct, TcPlugin(..), TcPluginResult (..),
                   ctEvidence, ctEvPred)
import TyCon      (TyCon)
import Type       (EqRel (NomEq), PredTree (EqPred),
                   classifyPredType)
#if __GLASGOW_HASKELL__ >= 711
import TyCoRep    (Type (..), TyLit (..))
#else
import TypeRep    (Type (..), TyLit (..))
#endif
{% endhighlight %}

Aside from the GHC API, and some Prelude modules, we also import some functions from my [ghc-tcplugins-extra](http://hackage.haskell.org/package/ghc-tcplugins-extra) package, `GHC.TcPluginM.Extra`: it provides some type-checker plugin utility functions with the intention to provide a stable API spanning multiple GHC releases.
As those of us who use the GHC API often know, the GHC API is fairly _unstable_, hence my personal need for [ghc-tcplugins-extra](http://hackage.haskell.org/package/ghc-tcplugins-extra).
Currently, [ghc-tcplugins-extra](http://hackage.haskell.org/package/ghc-tcplugins-extra) supports GHC 7.10 and GHC 8.0.

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

* `tcPluginInit`: Initialises the state used by our type-checker plugin.
  We will use it to look up the `TyCon` (type constructor) of the `GCD` type family.
  One can also perform IO operations in `tcPluginInit`, and have the state contain `IORef`s.
* `tcPluginSolve`: The main solver function, this is were we will actually perform the _gcd_ calculation.
* `tcPluginStop`: Cleaning up of the state.
  We have no use for it, but it can for example be used to remove files that are used as a communication medium with an SMT solver.

Our type-checker plugin solves equations involving our `GCD` type family, so we need to know that the types that we are dealing with actually involve `GCD`.
To do that, we need GHC's internal representation of `GCD`, which is a [TyCon](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/TyCon.html#t:TyCon).
We do the lookup of our `GCD` TyCon in the initialisation of our type-checker plugin, so that it becomes our plugin's internal state:

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

### Actually solving something

So we've done all the setting up for our type-checker plugin, and are now ready to implement the business end of it.
But what does a type-checker plugin actually do?
Well, it solves [Constraint](https://downloads.haskell.org/~ghc/7.10.3/docs/html/users_guide/constraint-kind.html)s, the types that in our Haskell code usually appear on the left of the `=>` arrow.

In our case, we will be solving equality constraints, that is, given the following haskell code:

{%highlight haskell %}
f :: Proxy (GCD 6 8) -> Proxy 2
f = id
{% endhighlight %}

we have to say whether the equality `GCD 6 8 ~ 2` can be _proven_.
So let's start with the type signature of our solver:

{%highlight haskell %}
solveGCD :: TyCon -- ^ GCD's TyCon
         -> [Ct]  -- ^ [G]iven constraints
         -> [Ct]  -- ^ [D]erived constraints
         -> [Ct]  -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
{% endhighlight %}

The first argument is the plugin's state, which in our case is just the (immutable) `TyCon` of our `GCD` type family.
Next come three categories of constraints that GHC's solver pipeline is giving to us:

* First are the given constraints, these are the proven facts about types available to the compiler.
* Second are the derived constraints... to be honest, I never really understood what they do, apparently they help with improving error messages, but I never investigated them in depth.
* And finally, there are the wanted constraints.
  These are the constraints that the compiler wants us to either:

  * Prove, so that the valid program type checks, or,
  * Disprove, so that it can give a good error message.
    That is, unsolved constraints also result in errors, but constraints that are outright rejected result in better error messages.

The result of our plugin is:

{%highlight haskell %}

data TcPluginResult
  = TcPluginContradiction [Ct]
  | TcPluginOk [(EvTerm,Ct)] [Ct]
{% endhighlight %}

where we can either return `TcPluginContradictions cts`, where `cts` are all the wanted constraints that we deemed insolvable (think `2 + 3 ~ 4` for ordinary natural numbers).
Or we can return `TcPluginOk proven newWanted`, where the `proven` are all the wanted constraints (`Ct`) that we solved together with with their proofs (`EvTerm`), and `newWanted` are all the new wanted constraints that must additionally be solved.

An example of the latter would be: given the constraint `x + GCD 6 8 ~ 2 + x`, my [ghc-typelits-natnormalise](http://hackage.haskell.org/package/ghc-typelits-natnormalise) plugin knows that addition is commutative, but doesn't know anything about `GCD`, so it would return: `TcPluginOK [("Believe me",x + GCD 6 8 ~ 2 + x)] [GCD 6 8 ~ 2]`.
The new wanted constraint, `GCD 6 8 ~ 2`, would then be picked up by the plugin that we are currently describing in this blog post.

Next, the body of our `solveGCD` function:

{%highlight haskell %}
solveGCD _     _ _ []      = return (TcPluginOk [] [])
solveGCD gcdTc _ _ wanteds = return $! case failed of
    [] -> TcPluginOk (mapMaybe (\c -> (,c) <$> evMagic c) solved) []
    f  -> TcPluginContradiction f
  where
    gcdWanteds :: [(Ct,(Integer,Integer))]
    gcdWanteds      = mapMaybe (toGCDEquality gcdTc) wanteds
    
    solved, failed :: [Ct]
    (solved,failed) = (map fst *** map fst)
                    $ partition (uncurry (==) . snd) gcdWanteds
{% endhighlight %}

The first clause basically says that, if there are no wanted constraints for us to solve we are done.
The thing is, our type-checker plugin is used inside two phases of GHC's solver pipeline.
In the first phase, only given (and probably derived) constraints are passed through the pipeline.
A type-checker plugin could then extend GHCs knowledge about the constraints by creating more given constraints, which could then help in solving future wanted constraints.
In the second phase, we'll be constantly given new wanted constraints that pop up during iterations of the solver, until all wanted constraints have passed trough the pipeline at least once.

This brings us to the second clause, which is the clause that handles this phase 2 part.
It gets all the wanted constraints (involving `GCD`) for which there is some hope that we actually can solve them; these interesting wanted constraints are `gcdWanteds`.
As we will see later on, the solving part basically consists of trying to reduce both sides of the equality relation to a number.
So given the equality `GCD 6 8 ~ 2`, we reduce it to `2 ~ 2`.
Consequently, all the `solved` constraints are the constraints where both sides of the equality reduced to the same number, and `failed` constraints are the constraints where the sides of the equality reduced to a different number.

Finally, when there are no `failed` constraints, we return all our solved constraints, together with a proof that the constraints hold; we don't return any new wanted constraints.
The proofs for the solved constraints are constructed by `evMagic` (which we will discuss later).
If there are any failed constraints, we return those in a `TcPluginContradiction`.

Next comes the code by which we find the constraints that are interesting for our plugin:

{%highlight haskell %}
toGCDEquality :: TyCon -> Ct -> Maybe (Ct,(Integer,Integer))
toGCDEquality gcdTc ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2
      -> (ct,) <$> ((,) <$> reduceGCD gcdTc t1
                        <*> reduceGCD gcdTc t2)
    _ -> Nothing

reduceGCD :: TyCon -> Type -> Maybe Integer
reduceGCD gcdTc = go
  where
    go (LitTy (NumTyLit i)) = Just i
    go (TyConApp tc [x,y])
      | tc == gcdTc = gcd <$> go x <*> go y
    go _ = Nothing
{% endhighlight %}

The `toGCDEquality` first looks at what type of constraint we are dealing with.
We are only interested in the equality constraints; other constraints are things like class constraints and implicit parameters.
If the constraint is an equality constraint (`EqPred`), we see if we can reduce both sides of the equality to a number using `reduceGCD`.

Finally, `reduceGCD` is the function that applies the actual `gcd` function!
If `reduceGCD` finds a type-level natural, it's done.
If it finds an application of our `GCD` type family, it first tries to reduce the argument, and subsequently applies the `gcd` function.
If it's not a type-level natural, or the application `GCD` type family, we give up.

### Burden of proof

The last part of our plugin is the evidence generation: the actual proof that the wanted equality relation holds.
This is sadly the part where we take the easy way out (the easy way is often pragmatic :-)).
We use the `evByFiat` function from [ghc-tcplugins-extra](http://hackage.haskell.org/package/ghc-tcplugins-extra), which, as its name implies, gives a proof by fiat.
The `evByFiat` function basically uses the same "proof", as the one that `unsafeCoerce` uses internally in GHC.

{%highlight haskell %}
evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "ghc-typelits-gcd" t1 t2)
    _                  -> Nothing
{% endhighlight %}

Perhaps these "proofs" by fiat are OK for the tiny type-checker plugin we have written here, perhaps it is not.
Looking at the [Agda code for GCD](https://github.com/aristidb/agda/blob/master/lib-0.5/src/Data/Nat/GCD.agda) it seems possible to provide a real proof for GCD.
Then again, Agda's proof uses inductively defined natural numbers, which we didn't have to begin with.

In the end, I use Haskell for actually building programs that run, or actually, building programs that get translated to digital circuits; and not to prove things about programs.
I'm fairly confident that the plugin we have written does not allow for incorrect programs to be type-checked (I know, assumptions are the mother of all ...), so I'm personally OK with these by fiat "proofs".

# Wrapping up

The complete code for this blog can be found on [https://github.com/christiaanb/ghc-typelits-gcd](https://github.com/christiaanb/ghc-typelits-gcd).
It contains the following, very minor, [test suite](https://github.com/christiaanb/ghc-typelits-gcd/blob/master/test/Spec.hs):

{%highlight haskell %}
test1 :: Proxy (GCD 6 8) -> Proxy 2
test1 = id

test2 :: Proxy (GCD 0 x) -> Proxy x
test2 = id
{% endhighlight %}

which, using our type-checker plugin, passes the type-checker, YAY!

When you start writing "real" type-checker plugins, you'll have to think _at least_ about the following aspects:

* How am I going to let given and wanted constraints interact?
* How do I ensure that I do not keep on generating new wanted constraints every time my plugin gets called?
  Because if you keep on generating new constraints, the GHCs solver will never finish, and compiling just becomes an exercise in watching a blinking cursor for all eternity.
* How do I interact with other type-checker plugins?
  Remember where I said that: given the constraint `x + GCD 6 8 ~ 2 + x`, our [ghc-typelits-natnormalise](http://hackage.haskell.org/package/ghc-typelits-natnormalise) plugin knows that addition is commutative, but doesn't know anything about `GCD`, so it would return: `TcPluginOK [("Believe me",x + GCD 6 8 ~ 2 + x)] [GCD 6 8 ~ 2]`.
  And the the GCD plugin would do the rest.
  That was actually a white lie!
  The evidence it generates is not our simple `evByFiat` (i.e. "Believe me"), but a _conditional_ evidence.
  We need it to be conditional, because if `GCD 6 8 ~ 2` turned out not to be true, then `x + GCD 6 8 ~ 2 + x` isn't true either.
  I used to solve this problem with [tricks/hacks](http://hackage.haskell.org/package/ghc-tcplugins-extra-0.2/docs/src/GHC-TcPluginM-Extra.html#newWantedWithProvenance), but GHC 8+ seems to allow me to do this in [a more sanctioned way](https://github.com/clash-lang/ghc-typelits-natnormalise/blob/master/src/GHC/TypeLits/Normalise.hs#L155).

Remember where I said it was easier to have the single equation definition of the `GCD` type family as opposed to the zero equation definition.
Our test case, `test2`, is the proof of that: our solver, as it is currently defined, is completely unable to handle `test2`.
Our current solver only works when both arguments of `GCD` reduce to a natural number, and gives up otherwise.

Indeed, this blog post was written with the intention of demonstrating a "working" prototype of a type-checker plugin with a "minimal" amount of code, and I think it serves that purpose well.
Given that there isn't much material on writing type-checker plugins out there, I understand there aren't too many type-checker plugins in the wild.
I hope this blog post serves as that final push for some of you to start writing your own type-checker plugins and further explore its design space.
