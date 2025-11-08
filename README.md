# Simple Data Processing (Core)

It's core package for `sdp`. Check the list of packages below.

## What's the point?

SDP is a way to work with data in a unified way. What used to be similar
functions with more or less the same behavior becomes a set of classes with
clear rules and restrictions.

Initially, `sdp` was created as a replacement for the standard `array` package,
with more functions and not requiring qualified imports.

In version `sdp-0.2`, the package contained many structures and classes that
were added to the main package to solve specific problems. This was justified
while I was studying at the university, because I had a lot of free time.
In the end, this complicated the support and development of the library, and I
abandoned it for a very long time. Now I am splitting it into several packages
to simplify the development process and gradually develop the project to solve
current problems.

In version 0.3 I partly returned to the old idea - the priority of modularity
over basic functionality. `sdp-core-0.3` will contain only basic classes and
some helping tools (for example, my implementation of TimSort).

## Quick start

sdp-core is a set of classes that solve problems with qualified imports.
To start using the library, add the following lines to your module:

```haskell
import Prelude ()
import SDP.SafePrelude
```

This is the easiest way to avoid function name conflicts (if I didn't forget
anything, lol).

`SDP.SafePrelude` exports:
* `sdp-core:SDP.Comparing`
* `sdp-core:SDP.Estimate`
* `base:Control.Monad.IO.Class`
* `base:Conrol.Monad.ST`
* `base:Contol.Monad`
* `base:Data.Functor.Classes`
* `base:Data.Bifunctor`
* `base:Data.Foldable`
* `(.)` and `id` from `base:Data.Category`
* `Semigroup` from `base:Data.Semigroup`, even for `base < 4.11`

`SDP.SafePrelude` hides these `Prelude` functions: `(.)`, `id`, `head`, `tail`,
`init`, `last`, `take`, `drop`, `(!!)`, `(++)`, `reverse`, `filter`, `lookup`,
`concat`, `concatMap`, `replicate`, `takeWhile`, `dropWhile`, `iterate`, `scanl`,
`scanr`, `scanl1`, `scanr1`, `zip`, `zip3`, `zipWith`, `zipWith3`, `readFile`,
`writeFile`, `appendFile`, `getContents`, `getChar`, `putChar`, `getLine`,
`putStr`, `putStrLn`

SDP.SafePrelude provides a number of additional functions: `liftA4`, `liftA5`,
`liftA6`, `joinM1`, `joinM2`, `joinM3`, `joinM4`, `joinM5`, `joinM6`, `stToMIO`,
`liftM6`.

### Classes

`sdp-core` generalize the most popular operations on linear (list-like) and
associative data structures including selection, splitting and sorting. With
`sdp-core` list functions aren't overlap their counterparts for other structures.

* `Nullable` is a service class of structures with null values.
* `Bordered` is a class of structures with borders and finite number of elements.
* `Estimate` is a service class for efficiently (at least, in finite time)
estimating the length of a structure or compare pair of structures by length (in
finite time if at least one of them is finite). Allows to express such
conditions as:
```haskell
xs .<. ys -- length xs < length ys
es .== 10 -- length es == 10
es .>   5 -- length es > 5
```
* `Unboxed` is a service class that simplifies interacting with data stored in
`ByteArray#` and `MutableArray#`.
* `Shape` is a service class for dimension operations and finite-dimensional
index transformations. `Index` is a service class that generalizes `Enum` to
interval operations, replaces `Ix`.
* `Linear` is a class of linear structures that generalizes the standard list
functions. `Split` is `Linear` extension, which implements additional list-like
operations like `split(At)` `takeWhile`, `isPrefixOf`, etc.
* `Indexed` is a class of indexed structures that generalizes read and modify
operations immutable structures.
* `Shaped` is a class of operations on structures generalized by the type of
index. Provides safe change of range and index type, fast extraction of
subsequences.
* `Map` is a class of operations on associative arrays (dictionaries).
* `Set` is a class of operations on sets.
* `Zip` is a class that generalizes zipping operations (see `zip`, `zipWith`,
`ZipList`, etc.).
* `Scan` is a class of convolutions with intermediate values.
* `Sort` is a sorting class for immutable structures.
* `BorderedM`, `LinearM`, `SplitM`, `IndexedM`, `SortM` - classes of operations
on mutable containers.

### Some technical information

`sdp` is traditionally shipped as a complement to `base`, not a replacement for
it (I find `rebase` and `rerebase` to be rather "toxic" names in relation to the
original).

#### Cross-platform support

I am a Linux programmer and I haven't had any other OS since 2018, except for a
couple of virtual machines. I rarely test anything outside of Linux, so the
library may work on Windows, Mac OS X or *BSD with some issues, especially
commits between releases. Feedback would be appreciated.

#### Dependencies

SDP supports Stackage LTS-7.0 and above, i.e. since GHC 8.0.* and base-4.9. This
is quite an old version, so there are some forks in the code. Although I'll be
honest - most of them are related to changes in the ghc-prim API, which has made
several big steps forward since I started writing the library long ago.

I could probably get rid of support for GHC 8.6 and below. Especially 8.0.2,
which is the one that gives me the biggest butthurt due to some compiler bugs
and the need to work around them. If backward compatibility gets in the way of
the next release, I will do so.

#### Backward compatibility

Regarding compatibility between `sdp` versions. It doesn't exist. Maybe it will
appear someday. In any case, I'm not the kind of person who will develop a
project gradually and carefully. Abandoning it for almost 5 years and rolling
out an update is much more my style.

#### Partially defined functions considered harmful

Now for the elephant in the room. Partially defined functions. I agree with
Snoyberg and his articles from late 2020 (lol) that this is very bad.

For context:
* [Haskell bad parts 1](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1)
* [Haskell bad parts 2](https://www.snoyman.com/blog/2020/11/haskell-bad-parts-2)
* [Haskell bad parts 3](https://www.snoyman.com/blog/2020/12/haskell-bad-parts-3)

I've added warnings to the most egregious cases and will add more as we go.
In my defense, `base-4.19`, which introduced some warnings, only made it into
Stackage LTS late last year (December 2024).

#### About Data.Default

Here I would really like to speak out about the dependence of `data-default-classes`
on `data-default`, but I lost the moral right to criticize other people's
decisions when I abandoned my project for 5 years. In fact, I do the exact
opposite with `sdp-core-0.3`.

The technical reason is that I don't need extra dependencies in the base package.

And frankly, I don't see any real use cases for the containers package in any of
my projects. So for me it's just a garbage package dependency.

> Perfection is finally attained not when there is no longer anything to add, but when there is no longer anything to take away (c) Antoine de Saint-Exupéry

#### About FMR

I originally intended this package as a convenient wrapper for FFI. Now I doubt
that the package has any future, because the limitations of the type system make
it very difficult to use in real code. It will not be used in `sdp` anytime soon.
Probably the same functionality can be achieved with `lens`.

### Roadmap?

* First, I just want to release `sdp-0.3`. It was supposed to be released in
2023, but I abandoned it because of my job at the time.
* Then I'll update the child packages and add some new ones.
* Once the main functionality is ready and all packages work with the current
LTS, I want to add `conduit` support. It will make my work on the next project
much easier.
* I'll probably leave sdp at 0.3 for a while and spend some time on
`Quasiquotation` and `PostgreSQL`. I like `persistent`, `esqueleto`, etc.,
but its current functionality is completely insufficient for my purposes.

## Versions

`sdp-core` follow [Haskell Package Versioning Policy](https://pvp.haskell.org).
To simplify the search for extensions, I also recommend the following rules:
* The `MAJOR` version of the derivative must match the smallest `MAJOR` version
of `sdp-core` with which it's compatible.
* Extensions should be called `sdp-%extensionname%` (e.g. `sdp-quickcheck`).
* Wrappers should be called `sdp4%libraryname%` (e.g., `sdp4text`).

## Using the SDP category

The `SDP` category is intended for `sdp-core` classes and primitives, as for
re-export of structures whose names are already taken in the `Data` category.
It shouldn't be used instead of `System`, `Control`, `Foreign`, etc.

## Contributing
For details of the process for submitting pull requests, please read
[CONTRIBUTING.md](https://github.com/andreymulik/sdp/blob/master/CONTRIBUTING.md).

## License
`sdp-core` is FOSS (free and open source software), you can redistribute it and/or
modify it under the terms of the BSD3 license. `sdp-core` is distributed in the
hope that it will be useful, but without any warranty, without even the implied
warranty of merchantability or fitness for a particular purpose. See the LICENSE
file for more details.

