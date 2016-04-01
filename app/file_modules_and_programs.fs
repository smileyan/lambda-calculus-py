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