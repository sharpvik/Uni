# Inductively Defined Sets

Let's create four kinds of entities:

1. Doobry
2. Whatsit
3. Thingamy
4. BlahBlah

And some rules in which they can be combined:

1. Doobry and Whatsit can be used on their own
2. Thingamy can be applied to a Doobry, a Whatsit, or a shape with another
   Thingamy ontop
3. BlahBlah can only be applied to a shape that has Thingamy as its top element

With these rules, we can construct an infinite inductively defined set of
elements that satisfy these criteria.

```
                                                 ( BlahBlah )
                       < Thingamy > < Thingamy > < Thingamy > ...
[ Doobry ] [ Whatsit ] [  Doobry  ] [ Whatsit  ] [  Doobry  ]
```

## Shapes and Sizes

The interesting thing is that since we know the rules and thus, know how
elements are constructed, we can derive for such set the generalised shapes that
represent the rules (sort of).

```
            (B)
        <T> <T>
[A] [B] {*} {*}
```

Where `{*}` represent some shape that follows the declared patterns. This
element is basically the place where we recurs down and explore. From here, we
can construct a grammar for this language.

```
E ::= Doobry | Whatsit | Thingamy E | BlahBlah (Thingamy E)
```

This grammar is the desciption of a language.

We can also write some derivation rules for a relation over Thingamys and
Whatsits. For example, let's describe "balanced" terms, where we call
expressions balanced if they have an equal number of Thingamys and BlahBlah.

```
                                                E
---------       ----------          ------------------------
|- Doobry       |- Whatsit          |- BlahBlah (Thingamy E)
```

So Doobry and Whatsit are balanced by default, and if we have a BlahBlah ontop
of a (Thingamy E), then we need to examine the E expression with the same rules
recursively to see if it's balanced or not.

There is no rule for Thingamy alone as this would not be balanced.
