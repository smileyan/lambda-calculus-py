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







