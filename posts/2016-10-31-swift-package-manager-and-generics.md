---
title: "Swift Package Manager and Generics"
date: 2016-10-31 10:07:43 -0500
tags: [swift, swift-package-manager, performance, generics]
---

A while back I had completed updating an old flood fill algorithm when I realized it would be a great opportunity to play with the Swift Package Manager. I set off to break apart the monolithic project into several standalone pieces that I could reuse within other projects. In the end I created two modules: one for the flood fill algorithm and another for the stack that the algorithm depended upon. Afterwards I created an application to house the flood fill module and output the result to disk. Everything appeared to be going very well!

The layout for the project dependencies were remarkably linear: the `FloodFillApp` depended upon the `FloodFill` module which depended upon the `Stack` module. Originally, without the separate modules, my image was taking around 200 milliseconds to process in debug mode and 40 milliseconds in release mode. I realized there might be some slight performance degradation due to `-whole-module-optimization` not being able to optimize as efficiently across module boundaries, however I wasn't quite prepared for a sudden jump to over *2 minutes* for my image! After double checking that I didn't miss some obscure implementation detail, I decided to try release mode. Running `swift build -c release` brought me down *16 seconds*. Huzzah?

After a few days of pain and toil, I came across the core of my problem: generics. I had attempted to make my `Stack` module something that could be recycled into other projects, so generics were the obvious solution. Unfortunately, after all of the tests I performed, there was no solution that would enable better performance with the project and modules "as-is". There are a couple of options to help with performance, but they each come with a steep cost.

1. Remove generics from the `Stack` module.
2. Move the `Stack` module inside the `FloodFill` module.
3. Use `@_specialize` on the generic class in the `Stack` module.

Let's take a look at each of those options.

Option 1's cost is removing generics: your module won't be as flexible or powerful as it could be using generics. This is impractical from a collection's standpoint. Imagine if Swift had the same crippling limitation put on something as significant as its array class: we'd have an IntArray, DoubleArray, and so forth. It'd be rubbish to develop a collection module around a single type, each of which utilizing the exact same code with only the type changing.

That said, removing generics resolves the performance issues significantly. Running this in release mode with separate modules yields a 50 millisecond execution time.

Option 2's cost is the equivalent of removing Swift Package Manager entirely and sticking everything back into the monolithic project. This, unfortunately, seems to be the best option. Without the ability to optimize for generics across module boundaries, we are right back to putting the project file in the workspace and manually linking the frameworks up from there.

Option 3's cost is specializing a generic with a *single known* concrete attribute. Of the three options I've found, I don't consider this a viable solution in any sense. Let me first give you an idea of what this decorator does: from the [Swift 'Generics.rst' document](https://github.com/apple/swift/blob/master/docs/Generics.rst#specialization):

>@_specialize currently acts as a hint to the optimizer, which generates type checks and code to dispatch to the specialized routine without affecting the signature of the generic function. The intention is to support efforts at evaluating the performance of specialized code. The performance impact is not guaranteed and is likely to change with the optimizer. This attribute should only be used in conjunction with rigorous performance analysis. Eventually, a similar attribute could be defined in the language, allowing it to be exposed as part of a function's API. That would allow direct dispatch to specialized code without type checks, even across modules.

The document also gives this code use example:

    struct S<T> {
      var x: T
      @_specialize(Int, Float)
      mutating func exchangeSecond<U>(_ u: U, _ t: T) -> (U, T) {
        x = t
        return (u, x)
      }
    }

    // Substitutes: <T, U> with <Int, Float> producing:
    // S<Int>::exchangeSecond<Float>(u: Float, t: Int) -> (Float, Int)

What I have found is that, yes, this does alleviate the problem that I've described in this post, however, *only the module dependency can call this*. This effectively means that only one type, the specialized type, will be performant–– everything else will suffer. If the `Stack` module were to use this decorator on the `push` and `pop` methods for the `Int` type, any other type, such as `Double`, would have the *same* terrible performance degradation that I've described.

That said, the performance of using `@_specialize` garnered a 90 millisecond execution time. That's over double the integrated, non-generic version of this project. When I originally separated everything into separate modules, this is roughly the level of performance that I had anticipated it being. For myself, it would be considered an acceptable performance penalty in order to maintain separate module boundaries and to isolate the scope of each project.
