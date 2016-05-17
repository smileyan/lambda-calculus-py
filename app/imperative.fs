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

        Note that num_buckets is a constant, which means our bucket array is of fixed length. 
        A practical implementation would need to be able to grow the array as the number of elements in the dictionary increases, but we'll omit this to simplify the presentation.

        The function hash_bucket is used throughout the rest of the module to choose the position in the array that a given key should be stored at. 
        It is implemented on top of Hashtbl.hash, which is a hash function provided by the OCaml runtime that can be applied to values of any type. 
        Thus, its own type is polymorphic: 'a -> int.

        The other functions defined above are fairly straightforward:

        create
        Creates an empty dictionary.

        length
        Grabs the length from the corresponding record field, thus returning the number of entries stored in the dictionary.

        find
        Looks for a matching key in the table and returns the corresponding value if found as an option.

        Another important piece of imperative syntax shows up in find: we write array.(index) to grab a value from an array. 
        find also uses List.find_map, which you can see the type of by typing it into the toplevel:

        # List.find_map;;
         - : 'a list -> f:('a -> 'b option) -> 'b option = <fun>

        List.find_map iterates over the elements of the list, calling f on each one until a Some is returned by f, at which point that value is returned. 
        If f returns None on all values, then None is returned.

        Now let's look at the implementation of iter:

        let iter t ~f =
          for i = 0 to Array.length t.buckets - 1 do
            List.iter t.buckets.(i) ~f:(fun (key, data) -> f ~key ~data)
          done

        iter is designed to walk over all the entries in the dictionary. 
        In particular, iter t ~f will call f for each key/value pair in dictionary t. 
        Note that f must return unit, since it is expected to work by side effect rather than by returning a value, and the overall iter function returns unit as well.

        The code for iter uses two forms of iteration: a for loop to walk over the array of buckets; and within that loop a call to List.iter to walk over the values in a given bucket. 
        We could have done the outer loop with a recursive function instead of a for loop, but for loops are syntactically convenient, and are more familiar and idiomatic in imperative contexts.

        The following code is for adding and removing mappings from the dictionary:

        let bucket_has_key t i key =
          List.exists t.buckets.(i) ~f:(fun (key',_) -> key' = key)

        let add t ~key ~data =
          let i = hash_bucket key in
          let replace = bucket_has_key t i key in
          let filtered_bucket =
            if replace then
              List.filter t.buckets.(i) ~f:(fun (key',_) -> key' <> key)
            else
              t.buckets.(i)
          in
          t.buckets.(i) <- (key, data) :: filtered_bucket;
          if not replace then t.length <- t.length + 1

        let remove t key =
          let i = hash_bucket key in
          if bucket_has_key t i key then (
            let filtered_bucket =
              List.filter t.buckets.(i) ~f:(fun (key',_) -> key' <> key)
            in
            t.buckets.(i) <- filtered_bucket;
            t.length <- t.length - 1
          )





