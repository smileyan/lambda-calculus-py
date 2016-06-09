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

  We can instantiate the functor by applying it to a module with the right signature. 
  In the following code, rather than name the module first and then call the functor, we provide the functor input as an anonymous module:

  # module Int_interval =
      Make_interval(struct
        type t = int
        let compare = Int.compare
      end);;
  module Int_interval :
    sig
      type t = Interval of int * int | Empty
      val create : int -> int -> t
      val is_empty : t -> bool
      val contains : t -> int -> bool
      val intersect : t -> t -> t
    end

  If the input interface for your functor is aligned with the standards of the libraries you use, 
  then you don't need to construct a custom module to feed to the functor. In this case, we can directly use the Int or String modules provided by Core:

  # module Int_interval = Make_interval(Int) ;;
  module Int_interval :
    sig
      type t = Make_interval(Core.Std.Int).t = Interval of int * int | Empty
      val create : int -> int -> t
      val is_empty : t -> bool
      val contains : t -> int -> bool
      val intersect : t -> t -> t
    end
  # module String_interval = Make_interval(String) ;;
  module String_interval :
    sig
      type t =
        Make_interval(Core.Std.String).t =
          Interval of string * string
        | Empty
      val create : string -> string -> t
      val is_empty : t -> bool
      val contains : t -> string -> bool
      val intersect : t -> t -> t
    end

  This works because many modules in Core, including Int and String, satisfy an extended version of the Comparable signature described previously. 
  Such standardized signatures are good practice, both because they make functors easier to use, 
  and because they encourage standardization that makes your codebase easier to navigate.

  We can use the newly defined Int_interval module like any ordinary module:

  # let i1 = Int_interval.create 3 8;;
  val i1 : Int_interval.t = Int_interval.Interval (3, 8)
  # let i2 = Int_interval.create 4 10;;
  val i2 : Int_interval.t = Int_interval.Interval (4, 10)
  # Int_interval.intersect i1 i2;;
  - : Int_interval.t = Int_interval.Interval (4, 8)

  This design gives us the freedom to use any comparison function we want for comparing the endpoints. 
  We could, for example, create a type of integer interval with the order of the comparison reversed, as follows:

  We can use the newly defined Int_interval module like any ordinary module:

  # let i1 = Int_interval.create 3 8;;
  val i1 : Int_interval.t = Int_interval.Interval (3, 8)
  # let i2 = Int_interval.create 4 10;;
  val i2 : Int_interval.t = Int_interval.Interval (4, 10)
  # Int_interval.intersect i1 i2;;
  - : Int_interval.t = Int_interval.Interval (4, 8)

  This design gives us the freedom to use any comparison function we want for comparing the endpoints. 
  We could, for example, create a type of integer interval with the order of the comparison reversed, as follows:

  # module Rev_int_interval =
      Make_interval(struct
        type t = int
        let compare x y = Int.compare y x
      end);;
  module Rev_int_interval :
    sig
      type t = Interval of int * int | Empty
      val create : int -> int -> t
      val is_empty : t -> bool
      val contains : t -> int -> bool
      val intersect : t -> t -> t
    end

  The behavior of Rev_int_interval is of course different from Int_interval:

  # let interval = Int_interval.create 4 3;;
  val interval : Int_interval.t = Int_interval.Empty
  # let rev_interval = Rev_int_interval.create 4 3;;
  val rev_interval : Rev_int_interval.t = Rev_int_interval.Interval (4, 3)

  Importantly, Rev_int_interval.t is a different type than Int_interval.t, even though its physical representation is the same. Indeed, the type system will prevent us from confusing them.

  # Int_interval.contains rev_interval 3;;
  Characters 22-34:
  Error: This expression has type Rev_int_interval.t
        but an expression was expected of type Int_interval.t

  This is important, because confusing the two kinds of intervals would be a semantic error, and it's an easy one to make. The ability of functors to mint new types is a useful trick that comes up a lot.

  Making the Functor Abstract

  There's a problem with Make_interval. The code we wrote depends on the invariant that the upper bound of an interval is greater than its lower bound, 
  but that invariant can be violated. The invariant is enforced by the create function, but because Interval.t is not abstract, we can bypass the create function:

  # Int_interval.is_empty (* going through create *)
      (Int_interval.create 4 3) ;;
  - : bool = true
  # Int_interval.is_empty (* bypassing create *)
      (Int_interval.Interval (4,3)) ;;
  - : bool = false

  To make Int_interval.t abstract, we need to restrict the output of Make_interval with an interface. Here's an explicit interface that we can use for that purpose:

  # module type Interval_intf = sig
      type t
      type endpoint
      val create : endpoint -> endpoint -> t
      val is_empty : t -> bool
      val contains : t -> endpoint -> bool
      val intersect : t -> t -> t
      end;;
    module type Interval_intf =
      sig
        type t
        type endpoint
        val create : endpoint -> endpoint -> t
        val is_empty : t -> bool
        val contains : t -> endpoint -> bool
        val intersect : t -> t -> t
      end

  This interface includes the type endpoint to give us a way of referring to the endpoint type. 
  Given this interface, we can redo our definition of Make_interval. Notice that we added the type endpoint to the implementation of the module to match Interval_intf:

  # module Make_interval(Endpoint : Comparable) : Interval_intf = struct
      type endpoint = Endpoint.t
      type t = | Interval of Endpoint.t * Endpoint.t
               | Empty

      ...

    end ;;
    module Make_interval : functor (Endpoint : Comparable) -> Interval_intf

  Sharing Constraints

    The resulting module is abstract, but it's unfortunately too abstract. 
    In particular, we haven't exposed the type endpoint, which means that we can't even construct an interval anymore:

    # module Int_interval = Make_interval(Int);;
    module Int_interval :
      sig
        type t = Make_interval(Core.Std.Int).t
        type endpoint = Make_interval(Core.Std.Int).endpoint
        val create : endpoint -> endpoint -> t
        val is_empty : t -> bool
        val contains : t -> endpoint -> bool
        val intersect : t -> t -> t
      end
    # Int_interval.create 3 4;;
    Characters 20-21:
    Error: This expression has type int but an expression was expected of type
            Int_interval.endpoint

    To fix this, we need to expose the fact that endpoint is equal to Int.t (or more generally, Endpoint.t, where Endpoint is the argument to the functor). 
    One way of doing this is through a sharing constraint, which allows you to tell the compiler to expose the fact that a given type is equal to some other type. 
    The syntax for a simple sharing constraint is as follows:

    <Module_type> with type <type> = <type'>

    The result of this expression is a new signature that's been modified so that it exposes the fact that type defined inside of the module type is equal to type' whose definition is outside of it. 
    One can also apply multiple sharing constraints to the same signature:

    <Module_type> with type <type1> = <type1'> and <type2> = <type2'>

    We can use a sharing constraint to create a specialized version of Interval_intf for integer intervals:

    # module type Int_interval_intf =
        Interval_intf with type endpoint = int;;
    module type Int_interval_intf =
      sig
        type t
        type endpoint = int
        val create : endpoint -> endpoint -> t
        val is_empty : t -> bool
        val contains : t -> endpoint -> bool
        val intersect : t -> t -> t
      end

    We can also use sharing constraints in the context of a functor. 
    The most common use case is where you want to expose that some of the types of the module being generated by the functor are related to the types in the module fed to the functor.

    In this case, we'd like to expose an equality between the type endpoint in the new module and the type Endpoint.t, from the module Endpoint that is the functor argument. We can do this as follows:

    # module Make_interval(Endpoint : Comparable)
          : (Interval_intf with type endpoint = Endpoint.t)
      = struct

        type endpoint = Endpoint.t
        type t = | Interval of Endpoint.t * Endpoint.t
                | Empty

        ...

      end ;;
    module Make_interval :
      functor (Endpoint : Comparable) ->
        sig
          type t
          type endpoint = Endpoint.t
          val create : endpoint -> endpoint -> t
          val is_empty : t -> bool
          val contains : t -> endpoint -> bool
          val intersect : t -> t -> t
        end




