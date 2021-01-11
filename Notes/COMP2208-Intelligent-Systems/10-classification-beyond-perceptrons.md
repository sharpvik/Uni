# Classification Beyond Perceptrons

## Supervised Learning

**Supervised Learning** is a learning strategy where we use the data labelled
by humans at the evaluation step in order to adjust the model (weight).

Here's the definition of labelled **Data**:

_D := {(x<sub>n</sub>, y<sub>n</sub>)}, n = 1, ..., N_

We feed _x<sub>n</sub>_ into the **Model** and it spits out the result
_y<sub>n</sub>'_.

_y<sub>n</sub>' = f(x<sub>n</sub>)_

Having this setup, we calculate the **Loss function** as follows:

_L = sum([ d(y<sub>n</sub>', y<sub>n</sub>) | n &larr; [1..N] ])_

where _d_ is the **distance function** s.t. _d(y', y) = 0 &iff; y = y'_.

If your result labels are discrete, then you are dealing with a
**classification model**, if they are continuous (like real numbers), it is a
**regression model**.

Formally, the task of learning is to minimise the **Loss function**.

## Unsupervised Learning

Sometimes the data is unlabelled. The goal here is to **discover patterns**,
capture some natural groupings of sorts.

- Group data into similar subsets (cluster _c<sub>n</sub>_)
- Inputs _x<sub>n</sub>_ mapped to output _c<sub>n</sub>_

Let's look at data for unsupervised algorithms:

_D := {x<sub>n</sub>}, n = 1, ..., N_

We feed _x_ into the **clustering** algorithm, and get the cluster label _c_
out.

_L = sum([ d(c<sub>n</sub>, x<sub>n</sub>) | n &larr; [1..N] ])_

The **distance function** in this case is defined as the distance between the
data point and its cluster centre.

**Representation learning** works through expressing data in a transformed manner: _z<sub>n</sub>_ captures "essense".

Use _z<sub>n</sub>_ to map _x<sub>n</sub>_ to itself. It works like compression.

To do that we put input _x_ through the **encoder** and **decoder**.

1. _z<sub>n</sub> = f<sub>e</sub>(x<sub>n</sub>)_
2. _x<sub>n</sub>' = f<sub>d</sub>(z<sub>n</sub>)_

_L = sum([ d(x<sub>n</sub>', x<sub>n</sub>) ])_

## Maximum Margin Classifier

It is often the case that there are multiple solutions to a perceptron problem
that is linearly separable. In other words, you can pick multiple different
lines that separate two classes without making mistakes.

In these cases, you may be interested in a **maximum margin classifier** that
will tell you how to draw two parallel lines between the two sets (ants and
beatles) s.t. there is as much distance (margin) between those lines as
mathematically possible.
