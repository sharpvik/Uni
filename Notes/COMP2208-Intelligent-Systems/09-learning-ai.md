# Learning AI (Intoduction)

Learning AI is so popular now, because it is very hard to replicate human
behaviour in domains where there is a lot of "noice" or non-determinism.

As an example, think about Shazam. It helps you to identify a song that's
playing in a restaurant by simply listening to it. It would be easy to write a
comparison algorithm that looks up a song given an MP3 recording of it. Just
compare the wave samples! However, what you serve to Shazam is far from an MP3.
There is a lot of noice in the surrounding environment, so their algorithms have
to be able to cleverly distinguish the song from everything else and find the
closest match. It would be very hard to provide a clear-cut traditional
algorithm to do that.

## Perceptron Learning Rule

Perceptrons learn from mistakes. They start with a random configuration and
respond to error notifications propagated back from the end state.

For problems of assortion in space, we can use simple vector rules to fix the
divide in that way.

## Decision Boundary Equation

_w<sup>T</sup>x = w<sub>0</sub> + w<sub>1</sub>x<sub>1</sub> + w<sub>2</sub>x<sub>2</sub> = 0_

This equaltion can now be expressed as a dot product of two vercotrs:
_x = [1 x<sub>1</sub> x<sub>2</sub>]<sup>T</sup>_ and
_w = [w<sup>0</sup> w<sup>1</sup> w<sup>2</sup>]<sup>T</sup>_

In this equation, _w<sup>0</sup>_ is the _y intersect_ of the line we are
describing. To find the coefficient of _x<sub>1</sub>_ we need to simply find
the slope _dy/dx_. Rearrange the result into the form above.

## Error Identification

If we have to classes of objects to separate, identified as _-1_ and _+1_, then
the product of predition and the expected answer is _1_ if the answer is correct
and _-1_ if we made an error. This is because whenever the signs are the same,
we'll get a positive result. The only time it is negative is when the signs are
different!

## Perceptron Algorithm (Update Step)

_w<sup>(t)</sup> &rarr; w<sup>(t+1)</sup> = w<sup>(t)</sup> &pm; nx_

For initial weight _w = [0.6 0.3 2.0]<sup>T</sup>_, and error value
_x<sub>e</sub> = [1 2.7 0.2]<sup>T</sup>_, update as follows:

_w - nx<sub>e</sub> = [0.6 0.3 2.0]<sup>T</sup> - (0.3)[1 2.7 0.2]<sup>T</sup>_

In this formula we assumed _n = 0.3_. The _n_ coefficient is our
_learning rate_. It tells us how much we overcompensate for our mistakes.

## Perceptron Limits

There are some problems that cannot be tackled by simple perceptrons. Such
problems are called **non linearly separable** because guess what -- they can't
be separated by a line in an acceptable way.

One such problem is the _XOR_ gate due to the weird symmetry of this function.

However, the more complex organisations of perceptrons actually _allow_ to solve
such problems. The trick is to map input space to some other space (basically
transforming the way it looks) through a **hidden layer** s.t. in the new space
it will be possible to separate using a straight line.

## Outcomes

After all this research, we moved on from the good-oldfashioned-AI that
represented state as _logical and rule-based_ to neural nets with
_internal representation_.
