// Chapter 4. Files, Modules, and Programs
    // SINGLE-FILE PROGRAMS
        # let assoc = [("one", 1); ("two",2); ("three",3)];;
        val assoc : (string * int) list = [("one", 1); ("two", 2); ("three", 3)]
        # List.Assoc.find assoc "two" ;;
        - : int option = Some 2
        # List.Assoc.add assoc "four" 4 (* add a new key*) ;;
        - : (string, int) List.Assoc.t =
        [("four", 4); ("one", 1); ("two", 2); ("three", 3)]
        # # List.Assoc.add assoc "two"  4 (* overwrite an existing key *) ;;
        - : (string, int) List.Assoc.t = [("two", 4); ("one", 1); ("three", 3)]
    // MULTIFILE PROGRAMS AND MODULES
    // SIGNATURES AND ABSTRACT TYPES
        val <identifier> : <type>
        open Core.Std
        
        (** Bump the frequency count for the given string. *)
        val touch : (string * int) list -> string -> (string * int) list
        
        Note that in the preceding example we use String.Map in some places and simply Map in others.
         This has to do with the fact that for some operations, like creating a Map.t, you need
          access to type-specialized information, and for others, like looking something up in Map.t,
           you don't. This is covered in more detail in Chapter 13, Maps and Hash Tables.
    // CONCRETE TYPES IN SIGNATURES
        In our frequency-count example, the module Counter had an abstract type Counter.t for
         representing a collection of frequency counts. Sometimes, you'll want to make a type
          in your interface concrete, by including the type definition in the interface.

        For example, imagine we wanted to add a function to Counter for returning the line
         with the median frequency count. If the number of lines is even, then there is no
          precise median, and the function would return the lines before and after the
           median instead. We'll use a custom type to represent the fact that there are
            two possible return values. Here's a possible implementation:
        
        type median = | Median of string
                      | Before_and_after of string * string
        
        let median t =
          let sorted_strings = List.sort (Map.to_alist t)
                                 ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
          in
          let len = List.length sorted_strings in
          if len = 0 then failwith "median: empty frequency count";
          let nth n = fst (List.length sorted_strings n) in
          if len mod 2 = 1
          then Median (nth (len/2))
          else Before_and_after (nth (len/2 -1), nth (len/2));;
    // NESTED MODULES

        Up until now, we've only considered modules that correspond to files, like counter.ml.
         But modules (and module signatures) can be nested inside other modules. As a simple
          example, consider a program that needs to deal with multiple identifiers like
           usernames and hostnames. If you just represent these as strings, then it becomes easy to confuse one with the other.

        A better approach is to mint new abstract types for each identifier, where those types are under the covers just implemented as strings.
         That way, the type system will prevent you from confusing a username with a hostname, and if you do need to convert,
          you can do so using explicit conversions to and from the string type.

        Here's how you might create such an abstract type, within a submodule:
        
        open Core.Std
        
        module Username : sig 
          type t
          val of_string : string -> t
          val to_string : t -> string
        end = struct
          type t = string
          let of_string x = x
          let to_string x = x
        end
        Note that the to_string and of_string functions above are implemented simply as the identity function,
         which means they have no runtime effect. They are there purely as part of the discipline
          that they enforce on the code through the type system.

        The basic structure of a module declaration like this is:
        module <name> : <signature> = <implementation>
        
        We could have written this slightly differently, by giving the signature its own top-level module type declaration,
         making it possible to create multiple distinct types with the same underlying implementation in a lightweight way:

        open Core.Std
        
        module type ID = sig
          type t
          val of_string : string -> t
          val to_string : t -> string
        end
        
        module String_id = struct
          type t = string
          let of_string x = x
          let to_string x = x
        end
        
        module Username : ID = String_id
        module Hostname : ID = String_id
        
        type session_info = { user: Username.t;
                              host: Hostname.t;
                              when_started: Time.t;
                            }
        
        let sessions_have_same_user s1 s2 =
          s1.user = s2.host
    OPENING MODULES
        Most of the time, you refer to values and types within a module by using the module name as an explicit qualifier.
        For example, you write List.map to refer to the map function in the List module.
        Sometimes, though, you want to be able to refer to the contents of a module without this explicit qualification.
        That's what the open statement is for.

        We've encountered open already, specifically where we've written open Core.Std to get access to the standard
        definitions in the Core library. In general, opening a module adds the contents of that module to the
        environment that the compiler looks at to find the definition of various identifiers. Here's an example:

            # module M = struct let foo = 3 end;;
             module M : sig val foo : int end
            # foo;;
             Characters -1-3:
             Error: Unbound value foo
            # open M;;
            
            # foo
             - : int = 3
        open is essential when you want to modify your environment for a standard library like Core,
        but it's generally good style to keep the opening of modules to a minimum.
        Opening a module is basically a trade-off between terseness and explicitness—the more modules you open,
        the fewer module qualifications you need, and the harder it is to look at an identifier and figure out
        where it comes from.

        Here's some general advice on how to deal with opens:

        Opening modules at the toplevel of a module should be done quite sparingly, and generally only with
        modules that have been specifically designed to be opened, like Core.Std or Option.Monad_infix.

        If you do need to do an open, it's better to do a local open. There are two syntaxes for local opens.
        For example, you can write:
            # let average x y =
                let open Int64 in
                x + y / int_of 2 ;;
             val average : int64 -> int64 -> int64 = <fun>
        An alternative to local opens that makes your code terser without giving up on explicitness is to 
        locally rebind the name of a module. So, when using the Counter.median type, instead of writing:
            # let print_median m =
                match m with
                | Counter.Median string -> printf "True median:\n   %s\n" string
                | Counter.Before_and_after (before, after) ->
                  printf "Before and after median:\n   %s\n   %s\n" before after

            # let print_median m =
                let module C = Counter in
                match m with
                | C.Median string -> printf "True median:\n   %s\n" string
                | C.Before_and_after (before, after) ->
                  printf "Before and after median:\n   %s\n   %s\n" before after
    INCLUDING MODULES
        While opening a module affects the environment used to search for identifiers, 
        including a module is a way of actually adding new identifiers to a module proper. 
        Consider the following simple module for representing a range of integer values:
            # module Interval = struct
                type t = | Interval of int * int
                         | Empty

                let create low high =
                  if low > high then Empty else Interval (low,high)
              end;;
             module Interval:
               sig type t = Interval of int * int | Empty var create : int -> int -> t end

        We can use the include directive to create a new, extended version of the Interval module:
        
         # module Extended_interval = struct
             include Interval
    
             let contains t x =
              match x with
              | Empty -> false
              | Interval (low,high) -> x >= low && x <= high
           end;;
          module Extended_interval : 
            sig
              type t = Interval.t = Interval int * int | Empty
              val create : int * int -> t
              val contains : t * int -> bool
            end
         # Extended_interval.contains (Extended_interval.create 3 10) 4;;
          - : bool = true
        
        The difference between include and open is that we've done more than change how identifiers are searched for: 
        we've changed what's in the module. If we'd used open, we'd have gotten a quite different result:

            # module Extended_interval = struct
                open Interval

                let contains t x =
                  match t with
                  | Empty -> false
                  | Interval (low,high) -> x >= low && x <= high
              end;;
             module Extended_interval :
               sig val contains : Extended_interval.t -> int -> bool end
            # Extended_interval.contains (Extended_interval.create 3 10) 4;;
             Characters 28-52:
             Error: Unbound value Extended_interval.create
        To consider a more realistic example, imagine you wanted to build an extended version of the List module, 
        where you've added some functionality not present in the module as distributed in Core. 
        include allows us to do just that:
            ext_list.ml
                # open Core.Std
                (* The new function we're going to add *)
                let rec intersperse list el =
                  match list with
                  | [] | [ _ ] -> list
                  | x :: y :: tl -> x :: el :: intersperse (y::tl) el
                (* The remainder of the list module *)
                include List
        Now, how do we write an interface for this new module? It turns out that include works on signatures as well, 
        so we can pull essentially the same trick to write our mli. The only issues is that we need to get our hands
        on the signature for the List module. This can be done using module type of, which computes a signature from a module:
            ext_list.mli
                open Core.Std
                
                (* Include the interface of the list module from Core *)
                include (module type of list)
                
                (* Signature of function we're adding *)
                val intersperse : 'a list -> 'a -> 'a list
        Note that the order of declarations in the mli does not need to match the order of declarations in the ml. 
        The order of declarations in the ml mostly matters insofar as it affects which values are shadowed.
        If we wanted to replace a function in List with a new function of the same name,
        the declaration of that function in the ml would have to come after the include List declaration.

        We can now use Ext_list as a replacement for List.
        If we want to use Ext_list in preference to List in our project, we can create a file of common definitions:

            common.ml
                module list = Ext_list
        And if we then put open Common after open Core.Std at the top of each file in our project, then references to List will automatically go to Ext_list instead. 

    COMMON ERRORS WITH MODULES
            When OCaml compiles a program with an ml and an mli, it will complain if it detects a mismatch between the two. Here are some of the common errors you'll run into.
        Type Mismatches
            The simplest kind of error is where the type specified in the signature does not match the type in the implementation of the module.
            As an example, if we replace the val declaration in counter.mli by swapping the types of the first two arguments:
                (** Bump the frequency count for the given string. *)
                val touch : string -> t -> t
            and we try to compile, we'll get the following error:
                corebuild freq.byte
        Missing Definitions
            We might decide that we want a new function in Counter for pulling out the frequency count of a given string. We can update the mli by adding the following line:
                val count : t -> string -> int
            Now, if we try to compile without actually adding the implementation, we'll get this error:
                $ corebuild freq.byte
                 File "counter.ml", line 1:
                 Error: The implementation counter.ml
                        does not match the interface counter.cmi:
                        The field `count' is required but not provided
                 Command exited with code 2.
            A missing type definition will lead to a similar error.
        Type Definition Mismatches
            Type definitions that show up in an mli need to match up with corresponding definitions in the ml.
            Consider again the example of the type median. The order of the declaration of variants matters to the OCaml compiler,
            so the definition of median in the implementation listing those options in a different order:
                (** Represents the median computed from a set of strings.  In the case where
                    there is an even number of choices, the one before and after the median is
                    returned.  *)
                type median = | Before_and_after of string * string
                              | Median of string
            will lead to a compilation error:
                $ corebuild freq.byte
                 File "counter.ml", line 1:
                 Error: The implementation counter.ml
                        does not match the interface counter.cmi:
                        Type declarations do not match:
                          type median = Median of string | Before_and_after of string * string
                        is not included in
                          type median = Before_and_after of string * string | Median of string
                        File "counter.ml", line 18, characters 5-84: Actual declaration
                        Fields number 1 have different names, Median and Before_and_after.
                 Command exited with code 2.
            Order is similarly important to other type declarations, including the order in which record fields are declared and 
            the order of arguments (including labeled and optional arguments) to a function.
        Cyclic Dependencies
            In most cases, OCaml doesn't allow cyclic dependencies, i.e., a collection of definitions that all refer to one another.
            If you want to create such definitions, you typically have to mark them specially. For example, when defining a set of 
            mutually recursive values (like the definition of is_even and is_odd in the section called “Recursive Functions”), you need to define them using let rec rather than ordinary let.

            The same is true at the module level. By default, cyclic dependencies between modules are not allowed, 
            and cyclic dependencies among files are never allowed. Recursive modules are possible but are a rare case, and we won't discuss them further here.

            The simplest example of a forbidden circular reference is a module referring to its own module name.
            So, if we tried to add a reference to Counter from within counter.ml:

                let singleton l = Counter.touch Counter.empty
            we'll see this error when we try to build:
                $ corebuild freq.byte
                 File "counter.ml", line 18, characters 18-31:
                 Error: Unbound module Counter
                 Command exited with code 2.
            The problem manifests in a different way if we create cyclic references between files.
            We could create such a situation by adding a reference to Freq from counter.ml, e.g., by adding the following line
                let _build_counts = Freq.build_counts
            In this case, ocamlbuild (which is invoked by the corebuild script) will notice the error and complain explicitly about the cycle:
                $ corebuild freq.byte
                 Circular dependencies: "freq.cmo" already seen in
                 [ "counter.cmo"; "freq.cmo" ]
    DESIGNING WITH MODULES
            The module system is a key part of how an OCaml program is structured.
            As such, we'll close this chapter with some advice on how to think about designing that structure effectively.
        Expose Concrete Types Rarely
            When designing an mli, one choice that you need to make is whether to expose the concrete definition of your types or leave them abstract. 
            Most of the time, abstraction is the right choice, for two reasons:
            it enhances the flexibility of your design, and it makes it possible to enforce invariants on the use of your module.

            Abstraction enhances flexibility by restricting how users can interact with your types,
            thus reducing the ways in which users can depend on the details of your implementation.
            If you expose types explicitly, then users can depend on any and every detail of the types you choose.
            If they're abstract, then only the specific operations you want to expose are available.
            This means that you can freely change the implementation without affecting clients, as long as you preserve the semantics of those operations.





