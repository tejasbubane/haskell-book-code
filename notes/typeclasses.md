# Typeclasses in Haskell

* Typeclasses define a set of operations.

* Typeclass instances define how that set of operations should be performed for the concrete type.

* Typeclass inheritance is a way of saying that while defining instance of this class - there must be a instance of the superclass already defined.

eg. in order to define an instance of `Ord` typeclass, there must be an instance of `Eq`. This logically makes sense because if we want to compare two values, we must be able to tell whether they are equal.
