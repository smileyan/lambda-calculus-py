Chapter 8. Imperative Programming

        Most of the code shown so far in this book, and indeed, most OCaml code in general, is pure. 
        Pure code works without mutating the program's internal state, performing I/O, reading the clock, or in any other way interacting with changeable parts of the world. 
        Thus, a pure function behaves like a mathematical function, always returning the same results when given the same inputs, 
        and never affecting the world except insofar as it returns the value of its computation. 
        Imperative code, on the other hand, operates by side effects that modify a program's internal state or interact with the outside world. 
        An imperative function has a new effect, and potentially returns different results, every time it's called.

        Pure code is the default in OCaml, and for good reason—it's generally easier to reason about, less error prone and more composable. 
        But imperative code is of fundamental importance to any practical programming language, because real-world tasks require that you interact with the outside world, 
        which is by its nature imperative. Imperative programming can also be important for performance. 
        While pure code is quite efficient in OCaml, there are many algorithms that can only be implemented efficiently using imperative techniques.

        OCaml offers a happy compromise here, making it easy and natural to program in a pure style, but also providing great support for imperative programming. 
        This chapter will walk you through OCaml's imperative features, and help you use them to their fullest.

    EXAMPLE: IMPERATIVE DICTIONARIES

        We'll start with the implementation of a simple imperative dictionary, i.e., a mutable mapping from keys to values. 
        This is really for illustration purposes; both Core and the standard library provide imperative dictionaries, and for most real-world tasks, you should use one of those implementations. 
        There's more advice on using Core's implementation in particular in Chapter 13, Maps and Hash Tables.

        The dictionary we'll describe now, like those in Core and the standard library, will be implemented as a hash table. 
        In particular, we'll use an open hashing scheme, where the hash table will be an array of buckets, each bucket containing a list of key/value pairs that have been hashed into that bucket.

        Here's the interface we'll match, provided as an mli. The type ('a, 'b) t represents a dictionary with keys of type 'a and data of type 'b:

        (* file: dictionary.mli *)
        open Core.Std

        type ('a, 'b) t

        val create : unit -> ('a, 'b) t
        val length : ('a, 'b) t -> int
        val add    : ('a, 'b) t -> key:'a -> data:'b -> unit
        val find   : ('a, 'b) t -> 'a -> 'b option
        val iter   : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit
        val remove : ('a, 'b) t -> 'a -> unit

        The mli also includes a collection of helper functions whose purpose and behavior should be largely inferrable from their names and type signatures. 
        Notice that a number of the functions, in particular, ones like add that modify the dictionary, return unit. This is typical of functions that act by side effect.

        We'll now walk through the implementation (contained in the corresponding ml file) piece by piece, explaining different imperative constructs as they come up.

        Our first step is to define the type of a dictionary as a record with two fields:

        (* file: dictionary.ml *)
        open Core.Std

        type ('a, 'b) t = { mutable length: int;
                            buckets: ('a * 'b) list array;
                          }

        The first field, length, is declared as mutable. In OCaml, records are immutable by default, but individual fields are mutable when marked as such. 
        The second field, buckets, is immutable but contains an array, which is itself a mutable data structure.

        Now we'll start putting together the basic functions for manipulating a dictionary:

        let num_buckets = 17

        let hash_bucket key = (Hashtbl.hash key) mod num_buckets

        let create () =
          { length = 0;
            buckets = Array.create ~len:num_buckets [];
          }

        let length t = t.length

        let find t key =
          List.find_map t.buckets.(hash_bucket key)
            ~f:(fun (key',data) -> if key' = key then Some data else None)



