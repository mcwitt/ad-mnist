# ad-mnist

Exploring some interesting topics via a simple neural network classifier for [MNIST][mnist].

- _Dependent types_ enable static checking of matrix dimensions, making implementation of e.g. backprop much less error prone. Here I use `Numeric.LinearAlgebra.Static` in the [hmatrix][hmatrix] package.
- _Automatic differentiation_ to avoid writing manual code for backpropagation. While improved by the use of dependent types, manual implementation is still relatively error prone, tedious, and creates substantial friction when experimenting with different model architectures. Initially I'm experimenting with the [ad][ad] package. Another interesting one is [backprop][backprop].
- _GPU acceleration_ (future goal): [accelerate][accelerate] seems like an interesting choice here (but seems not possible to use with existing AD libraries, [yet](https://github.com/AccelerateHS/accelerate/issues/398?))

Inspired by some of Justin Le's
[blog](https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html)
[posts](https://blog.jle.im/entry/practical-dependent-types-in-haskell-2.html)
on the subject.

[mnist]: http://yann.lecun.com/exdb/mnist/
[hmatrix]: https://hackage.haskell.org/package/hmatrix
[ad]: https://hackage.haskell.org/package/ad
[backprop]: https://hackage.haskell.org/package/backprop
[accelerate]: https://hackage.haskell.org/package/accelerate
