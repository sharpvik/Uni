# Intelligent Agents

> An **agent** is anything that can be viewed as perceiving its **environment**
> through **sensors** and acting upon that environment through **actuators**.

> **Percept** is agent's perceptual input at any given instant. Sort of like
> reflection of their environment's state through agent's sensors.

> An egent's **percept sequence** is the complete history of everything the
> agent has ever perceived.

An agent's choice of action can depend only on the stuff that they've perceived;
not on the things that they haven't observed.

Agent's behaviour is described by the **agent function** that maps any given
percept sequence to an action. It is an _abstract_ mathematical concept.
Internally, the agent function will be _implemented_ as an **agent program**.

## Rationality

> A **rational agent** is one that does the right thing... but what does it mean
> to do "the right thing"?

In AI we say that correctness is defined as the difference between desired
outcomes and real consequences of the agent's behaviour. The notion of
desirability is captured by a **performance measure** that evaluates given
sequence of environment states.

> It is better to design performance measures according to what one actually
> wants in the environment, rather than according to how one thinks the agent
> shoul behave.

If you award 1 point each time an AI vacuum-cleaner picks up dust, it may very
well decide to run the following actions in a loop to get more points:

1. Pick up some dust;
2. Spit it out;
3. Pick it up again;
4. Spit it out... and so on.

This is not what we want! We want the room to be clean in a way that makes our
electricity bill as small as possible.

This leads to a **definition of a rational agent**:

> For each possible percept sequence, a rational agent should select an action
> that is expected to maximize its performance measure, given the evidence
> provided by the percept sequence and whatever built-in knowledge the agent
> has.
