Chapter 9. Functors
  Up until now, we've seen OCaml's modules play an important but limited role. 
  In particular, we've seen them as a mechanism for organizing code into units with specified interfaces. 
  But OCaml's module system can do much more than that, serving as a powerful tool for building generic code and structuring large-scale systems. 
  Much of that power comes from functors.

  Functors are, roughly speaking, functions from modules to modules, and they can be used to solve a variety of code-structuring problems, including:

  Dependency injection
  Makes the implementations of some components of a system swappable. 
  This is particularly useful when you want to mock up parts of your system for testing and simulation purposes.

  Autoextension of modules
  Functors give you a way of extending existing modules with new functionality in a standardized way. 
  For example, you might want to add a slew of comparison operators derived from a base comparison function. 
  To do this by hand would require a lot of repetitive code for each type, but functors let you write this logic just once and apply it to many different types.

  Instantiating modules with state
  Modules can contain mutable states, and that means that you'll occasionally want to have multiple instantiations of a particular module, each with its own separate and independent mutable state. 
  Functors let you automate the construction of such modules.

  These are really just some of the uses that you can put functors to. 
  We'll make no attempt to provide examples of all of the uses of functors here. 
  Instead, this chapter will try to provide examples that illuminate the language features and design patterns that you need to master in order to use functors effectively.

  A TRIVIAL EXAMPLE

    Let's create a functor that takes a module containing a single integer variable x and returns a new module with x incremented by one. 
    This is intended to serve as a way to walk through the basic mechanics of functors, even though it's not something you'd want to do in practice.

    First, let's define a signature for a module that contains a single value of type int:

    # module type X_int = sig val x : int end;;
    module type X_int = sig val x : int end

    Now we can define our functor. We'll use X_int both to constrain the argument to the functor and to constrain the module returned by the functor:

    # module Increment (M : X_int) : X_int = struct
        let x = M.x + 1
      end;;
    module Increment : functor (M : X_int) -> X_int

    One thing that immediately jumps out is that functors are more syntactically heavyweight than ordinary functions. 
    For one thing, functors require explicit (module) type annotations, which ordinary functions do not. 
    Technically, only the type on the input is mandatory, although in practice, you should usually constrain the module returned by the functor, 
    just as you should use an mli, even though it's not mandatory.

    The following shows what happens when we omit the module type for the output of the functor:

    # module Increment (M : X_int) = struct
        let x = M.x + 1
      end;;
    module Increment : functor (M : X_int) -> sig val x : int end

    We can see that the inferred module type of the output is now written out explicitly, 
    rather than being a reference to the named signature X_int.

    We can use Increment to define new modules:

    # module Three = struct let x = 3 end;;
    module Three : sig val x : int end
    # module Four = Increment(Three);;
    module Four : sig val x : int end
    # Four.x - Three.x;;
    - : int = 1

    In this case, we applied Increment to a module whose signature is exactly equal to X_int. 
    But we can apply Increment to any module that satisfies the interface X_int, in the same way that the contents of an ml file must satisfy the mli. 
    That means that the module type can omit some information available in the module, either by dropping fields or by leaving some fields abstract. 
    Here's an example:

    # module Three_and_more = struct
        let x = 3
        let y = "three"
    end;;
    module Three_and_more : sig val x : int val y : string end
    # module Four = Increment(Three_and_more);;
    module Four : sig val x : int end

    The rules for determining whether a module matches a given signature are similar in spirit to the rules in an object-oriented language 
    that determine whether an object satisfies a given interface. 
    As in an object-oriented context, the extra information that doesn't match the signature you're looking for (in this case, the variable y) is simply ignored.

  A BIGGER EXAMPLE: COMPUTING WITH INTERVALS

    Let's consider a more realistic example of how to use functors: a library for computing with intervals. 
    Intervals are a common computational object, and they come up in different contexts and for different types. 
    You might need to work with intervals of floating-point values or strings or times, and in each of these cases, 
    you want similar operations: testing for emptiness, checking for containment, intersecting intervals, and so on.

    Let's see how to use functors to build a generic interval library that can be used with any type 
    that supports a total ordering on the underlying set over which you want to build intervals.

    First we'll define a module type that captures the information we'll need about the endpoints of the intervals. 
    This interface, which we'll call Comparable, contains just two things: a comparison function and the type of the values to be compared:

    # module type Comparable = sig
        type t
        val compare : t -> t -> int
      end ;;
    module type Comparable = sig type t val compare : t -> t -> int end

    The comparison function follows the standard OCaml idiom for such functions, 
    returning 0 if the two elements are equal, a positive number if the first element is larger than the second, 
    and a negative number if the first element is smaller than the second. Thus, we could rewrite the standard comparison functions on top of compare.

    compare x y < 0     (* x < y *)
    compare x y = 0     (* x = y *)
    compare x y > 0     (* x > y *)

    (This idiom is a bit of a historical error. 
    It would be better if compare returned a variant with three cases for less than, greater than, and equal. 
    But it's a well-established idiom at this point, and unlikely to change.)

    The functor for creating the interval module follows. 
    We represent an interval with a variant type, which is either Empty or Interval (x,y), where x and y are the bounds of the interval. 
    In addition to the type, the body of the functor contains implementations of a number of useful primitives for interacting with intervals:

    # module Make_interval(Endpoint : Comparable) = struct

      type t = | Interval of Endpoint.t * Endpoint.t
              | Empty

      (** [create low high] creates a new interval from [low] to
          [high].  If [low > high], then the interval is empty *)
      let create low high =
        if Endpoint.compare low high > 0 then Empty
        else Interval (low,high)

      (** Returns true iff the interval is empty *)
      let is_empty = function
        | Empty -> true
        | Interval _ -> false

      (** [contains t x] returns true iff [x] is contained in the
          interval [t] *)
      let contains t x =
        match t with
        | Empty -> false
        | Interval (l,h) ->
          Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

      (** [intersect t1 t2] returns the intersection of the two input
          intervals *)
      let intersect t1 t2 =
        let min x y = if Endpoint.compare x y <= 0 then x else y in
        let max x y = if Endpoint.compare x y >= 0 then x else y in
        match t1,t2 with
        | Empty, _ | _, Empty -> Empty
        | Interval (l1,h1), Interval (l2,h2) ->
          create (max l1 l2) (min h1 h2)

    end ;;
  module Make_interval :
    functor (Endpoint : Comparable) ->
      sig
        type t = Interval of Endpoint.t * Endpoint.t | Empty
        val create : Endpoint.t -> Endpoint.t -> t
        val is_empty : t -> bool
        val contains : t -> Endpoint.t -> bool
        val intersect : t -> t -> t
      end
