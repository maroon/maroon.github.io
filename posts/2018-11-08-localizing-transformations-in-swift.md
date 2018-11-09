---
title: Localizing Transformations in Swift
date: 2018-11-08 13:13:03 -0600
tags: swift
---

The other day I was dealing with `CATransform3D` and found prototyping animations with it to be rather cumbersome. It wasn't that the animations were difficult, or even hard to understand, rather it was that I found myself with far more clutter than I could keep track of. Simply rotating two views around a center point became tedious as both required slightly different variations of one another and thus were polluting the namespace with overlapping variables. Variables that I'd have to reassign, mutate, or name as one-offs. Creating a function was a bit over-the-top as I was still playing with ideas; I didn't want anything concrete, nor did I want to waste time working out a common abstraction.

It was then that I found myself longing for days where I could create a new scope in C and be done with it. I began wondering what exactly that would look like in Swift. How would I go about creating something like that? I knew that I couldn't just write an extension for `Any` as it is a non-nominal type, nor could I just throw it onto `AnyObject` because that would exclude `struct` types.

What I wanted was simple: a scope operator. The problem is that I have no way of representing that within the confines of the language. So what was I _really_ after?

  1. I would like semi-local variables that overlap in name, but don't conflict and can remain immutable.
  2. I want to deal with a convenient variable name rather than something like `view.layer.transform`.
  3. I would like not to worry about an intermediary variable to assign the mutated value to.
  4. I want to be able to specify a scope without creating new function definitions everywhere. 

The first item can be a little confusing for those that haven't run into C scopes before. I'm going to express it here in Objective-C.

``` objective-c
{
  CGPoint point = CGPointMake(5, 12);
  {
    CGPoint point = CGPointMake(44, -11);
    NSLog(@"%@", NSStringFromCGPoint(point));
  }
  NSLog(@"%@", NSStringFromCGPoint(point));
}
```

Outputs: 

``` objective-c
{44, -11}
{5, 12}
```

For me, being able to rapidly scope out details like this is an immeasurably powerful tool.

I started toying with the shape of these ideas by writing a function that would take a value and transform it.

``` swift
let f: (CATransform3D) -> CATransform3D = { transform in
  // Mutate
  return transform
}
```

That's a fine start, but there is a problem: I still would have to mess with intermediary variables. I couldn't just tell the closure that I wanted to mutate the variable `transform`. [Rest in peace, my sweet prince.](https://github.com/apple/swift-evolution/blob/master/proposals/0003-remove-var-parameters.md) I'd have to create a new variable that was mutable within the scope of the closure.

``` swift
let f: (CATransform3D) -> CATransform3D = { transform in
  var transform = transform
  // Mutate
  return transform
}
```

That's pretty ugly. This design does say something about where to go from here, though. That says to me that I want to reassign the mutated value back to the original input. OK, that's an easy enough change.

``` swift
let f: (inout CATransform3D) -> Void = { transform in
  // Mutate
}
```

That is a _lot_ closer to what I actually want. This also knocks out the first 3 items on the list! That last one though–– that's going to be tough. Maybe with just a little luck and a bit of finesse I can get rid of the function variable name and...

``` swift
_ = { (transform: inout CATransform3D) -> Void in
  // Mutate
}(&currentFace.layer.transform)
```

[Perfection.](https://giphy.com/gifs/x-men-michael-fassbender-gdHpm6yTnzYJO)

No. Just kidding. That's downright awful. Arguably worse than where it just was.

Looking closely at how it is being expressed does give a hint about what is needed. This is forcing an assignment. Without `_ =` it won't compile. Rather than throwing out a function that mutates a value, it might be better expressed as an operator.

I'm not really a fan of creating new operators, as most of the time I feel it can rapidly pollute the namespace with awkward or unnecessary structures. Used sparingly and when appropriate, I find them to be remarkably expressive.

``` swift
infix operator =>
func => <T>(value: inout T, transform: (inout T) throws -> Void) rethrows {
  try transform(&value)
}
```

This will take some left-hand `value` and apply `transform` to it. Since the left-hand `value` is `T`, both of the parameters that involve `T` will require `inout` to be applied in order to permit mutation.

``` swift
view.layer.transform => { transform in
  transform = CATransform3DTranslate(transform, x, y, 0.0)
  transform = CATransform3DRotate(transform, rotation, 1, 0, 0)
  transform = CATransform3DTranslate(transform, x, y, 0.0)
}
```

Now all of the items on my list are crossed off and I have a very straightforward transform applied to the structure that I ultimately wanted to mutate.
