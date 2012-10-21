# Structure and Interpretation of Classical Mechanics

## What is this project?

At its core, it is an implementation of the book *Structure and Interpretation of Classical Mechanics* by Gerald Jay Sussman and Jack Wisdom, in Haskell.

This involves creating a Haskell implementation of the Scheme library *scmutils*, which includes toolboxes for symbolic algebra, differentiation and integration.

A key feature is that the routines for differentation and integration are agnostic to the underlying type - therefore the same functions are overloaded to work with symbolic exprexpressions or with numerical quantities. For example, we could define a polynomial function

    >>> let f a = (1+a)^3

which applies equally to numbers and symbolic expressions:

    >>> f 2
    27
    >>> f x
    1.0 + 3.0x + 3.0x² + x³

We can also find the first four derivates both numerically and symbolically:

    >>> dTake 4 $ f (dVar 2)
    [27,27,18,16]
    >>> dTake 4 $ f (dVar x)
    [1.0 + 3.0x + 3.0x² + x³,3.0 + 6.0x + 3.0x²,6.0 + 6.0x,6.0]

Eventually the library will include type-agnostic integration routines as well.

Get the book [here](http://mitpress.mit.edu/sicm/).

## Who is it by?

Chris Taylor. You can find me at [Stack Overflow](http://stackoverflow.com/users/546084/chris-taylor) or [Math.StackExchange](http://math.stackexchange.com/users/4873/chris-taylor) or look at my [LinkedIn profile](http://www.linkedin.com/profile/view?id=19184234) or follow me on [Twitter](https://twitter.com/crntaylor).
