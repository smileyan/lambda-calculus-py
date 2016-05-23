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

        This preceding code is made more complicated by the fact that we need to detect whether we are overwriting or removing an existing binding, so we can decide whether t.length needs to be changed. 
        The helper function bucket_has_key is used for this purpose.

        Another piece of syntax shows up in both add and remove: the use of the <- operator to update elements of an array (array.(i) <- expr) and for updating a record field (record.field <- expression).

        We also use ;, the sequencing operator, to express a sequence of imperative actions. We could have done the same using let bindings:

        let () = t.buckets.(i) <- (key, data) :: filtered_bucket in
          if not replace then t.length <- t.length + 1

        but ; is more concise and idiomatic. More generally,

        <expr1>;
        <expr2>;
        ...
        <exprN>

        is equivalent to

        let () = <expr1> in
        let () = <expr2> in
        ...
        <exprN>

        When a sequence expression expr1; expr2 is evaluated, expr1 is evaluated first, and then expr2. 
        The expression expr1 should have type unit (though this is a warning rather than a hard restriction. The -strict-sequence compiler flag makes this a hard restriction, which is generally a good idea), 
        and the value of expr2 is returned as the value of the entire sequence. For example, the sequence print_string "hello world"; 1 + 2 first prints the string "hello world", then returns the integer 3.

        Note also that we do all of the side-effecting operations at the very end of each function. 
        This is good practice because it minimizes the chance that such operations will be interrupted with an exception, leaving the data structure in an inconsistent state.

    PRIMITIVE MUTABLE DATA

        Now that we've looked at a complete example, let's take a more systematic look at imperative programming in OCaml. 
        We encountered two different forms of mutable data above: records with mutable fields and arrays. 
        We'll now discuss these in more detail, along with the other primitive forms of mutable data that are available in OCaml.

      Array-Like Data
        OCaml supports a number of array-like data structures; i.e., mutable integer-indexed containers that provide constant-time access to their elements. We'll discuss several of them in this section.

      Ordinary arrays

        The array type is used for general-purpose polymorphic arrays. The Array module has a variety of utility functions for interacting with arrays, including a number of mutating operations. 
        These include Array.set, for setting an individual element, and Array.blit, for efficiently copying values from one range of indices to another.

        Arrays also come with special syntax for retrieving an element from an array:

          <array_expr>.(<index_expr>)

        and for setting an element in an array:

          <array_expr>.(<index_expr>) <- <value_expr>

        Out-of-bounds accesses for arrays (and indeed for all the array-like data structures) will lead to an exception being thrown.

        Array literals are written using [| and |] as delimiters. Thus, [| 1; 2; 3 |] is a literal integer array.

      Strings

        Strings are essentially byte arrays which are often used for textual data. 
        The main advantage of using a string in place of a Char.t array (a Char.t is an 8-bit character) is that the former is considerably more space-efficient; 
        an array uses one word—8 bytes on a 64-bit machine—to store a single entry, whereas strings use 1 byte per character.

        Strings also come with their own syntax for getting and setting values:

          <string_expr>.[<index_expr>]
          <string_expr>.[<index_expr>] <- <char_expr>

        And string literals are bounded by quotes. There's also a module String where you'll find useful functions for working with strings.

      Bigarrays

        A Bigarray.t is a handle to a block of memory stored outside of the OCaml heap. 
        These are mostly useful for interacting with C or Fortran libraries, and are discussed in Chapter 20, Memory Representation of Values. 
        Bigarrays too have their own getting and setting syntax:

        <bigarray_expr>.{<index_expr>}
        <bigarray_expr>.{<index_expr>} <- <value_expr>

      Mutable Record and Object Fields and Ref Cells

        As we've seen, records are immutable by default, but individual record fields can be declared as mutable. 
        These mutable fields can be set using the <- operator, i.e., record.field <- expr.

        As we'll see in Chapter 11, Objects, fields of an object can similarly be declared as mutable, and can then be modified in much the same way as record fields.

      Ref cells

        Variables in OCaml are never mutable—they can refer to mutable data, but what the variable points to can't be changed. 
        Sometimes, though, you want to do exactly what you would do with a mutable variable in another language: define a single, mutable value. 
        In OCaml this is typically achieved using a ref, which is essentially a container with a single mutable polymorphic field.

        The definition for the ref type is as follows:

        # type 'a ref = { mutable contents : 'a };;
         type 'a ref = { mutable contents : 'a; }

        The standard library defines the following operators for working with refs.

        ref expr
        Constructs a reference cell containing the value defined by the expression expr.

        !refcell
        Returns the contents of the reference cell.

        refcell := expr
        Replaces the contents of the reference cell.

        You can see these in action:

        # let x = ref 1;;
         val x : int ref = {contents = 1}
        #   !x;;
         - : int = 1
        #   x := !x + 1;;
         - : unit = ()
        #   !x;;
         - : int = 2

        The preceding are just ordinary OCaml functions, which could be defined as follows:

        # let ref x = { contents = x };;
         val ref : 'a -> 'a ref = <fun>
        #   let (!) r = r.contents;;
         val ( ! ) : 'a ref -> 'a = <fun>
        #   let (:=) r x = r.contents <- x;;
         val ( := ) : 'a ref -> 'a -> unit = <fun>

      Foreign Functions

        Another source of imperative operations in OCaml is resources that come from interfacing with external libraries through OCaml's foreign function interface (FFI). 
        The FFI opens OCaml up to imperative constructs that are exported by system calls or other external libraries. 
        Many of these come built in, like access to the write system call or to the clock, while others come from user libraries, like LAPACK bindings. 
        OCaml's FFI is discussed in more detail in Chapter 19, Foreign Function Interface.

      FOR AND WHILE LOOPS

        OCaml provides support for traditional imperative looping constructs, in particular, for and while loops. 
        Neither of these constructs is strictly necessary, since they can be simulated with recursive functions. 
        Nonetheless, explicit for and while loops are both more concise and more idiomatic when programming imperatively.

        The for loop is the simpler of the two. Indeed, we've already seen the for loop in action—the iter function in Dictionary is built using it. Here's a simple example of for:

          # for i = 0 to 3 do printf "i = %d\n" i done;;


            i = 0
            i = 1
            i = 2
            i = 3
            - : unit = ()

        As you can see, the upper and lower bounds are inclusive. We can also use downto to iterate in the other direction:

          # for i = 3 downto 0 do printf "i = %d\n" i done;;


            i = 3
            i = 2
            i = 1
            i = 0
            - : unit = ()

        Note that the loop variable of a for loop, i in this case, is immutable in the scope of the loop and is also local to the loop, i.e., it can't be referenced outside of the loop.

        OCaml also supports while loops, which include a condition and a body. 
        The loop first evaluates the condition, and then, if it evaluates to true, evaluates the body and starts the loop again. 
        Here's a simple example of a function for reversing an array in place:

          # let rev_inplace ar =
              let i = ref 0 in
              let j = ref (Array.length ar - 1) in
              (* terminate when the upper and lower indices meet *)
              while !i < !j do
                (* swap the two elements *)
                let tmp = ar.(!i) in
                ar.(!i) <- ar.(!j);
                ar.(!j) <- tmp;
                (* bump the indices *)
                incr i;
                decr j
              done
            ;;
          val rev_inplace : 'a array -> unit = <fun>
          # let nums = [|1;2;3;4;5|];;
          val nums : int array = [|1; 2; 3; 4; 5|]
          # rev_inplace nums;;
          - : unit = ()
          # nums;;
          - : int array = [|5; 4; 3; 2; 1|]

        In the preceding example, we used incr and decr, which are built-in functions for incrementing and decrementing an int ref by one, respectively.

      EXAMPLE: DOUBLY LINKED LISTS

        Another common imperative data structure is the doubly linked list. 
        Doubly linked lists can be traversed in both directions, and elements can be added and removed from the list in constant time. 
        Core defines a doubly linked list (the module is called Doubly_linked), but we'll define our own linked list library as an illustration.

        Here's the mli of the module we'll build:

        (* file: dlist.mli *)
        open Core.Std

        type 'a t
        type 'a element

        (** Basic list operations  *)
        val create   : unit -> 'a t
        val is_empty : 'a t -> bool

        (** Navigation using [element]s *)
        val first : 'a t -> 'a element option
        val next  : 'a element -> 'a element option
        val prev  : 'a element -> 'a element option
        val value : 'a element -> 'a

        (** Whole-data-structure iteration *)
        val iter    : 'a t -> f:('a -> unit) -> unit
        val find_el : 'a t -> f:('a -> bool) -> 'a element option

        (** Mutation *)
        val insert_first : 'a t -> 'a -> 'a element
        val insert_after : 'a element -> 'a -> 'a element
        val remove : 'a t -> 'a element -> unit

        Note that there are two types defined here: 'a t, the type of a list; and 'a element, the type of an element. 
        Elements act as pointers to the interior of a list and allow us to navigate the list and give us a point at which to apply mutating operations.

        Now let's look at the implementation. We'll start by defining 'a element and 'a t:

        (* file: dlist.ml *)
        open Core.Std

        type 'a element =
          { value : 'a;
            mutable next : 'a element option;
            mutable prev : 'a element option
          }

        type 'a t = 'a element option ref

        An 'a element is a record containing the value to be stored in that node as well as optional (and mutable) fields pointing to the previous and next elements. 
        At the beginning of the list, the prev field is None, and at the end of the list, the next field is None.

        The type of the list itself, 'a t, is a mutable reference to an optional element. This reference is None if the list is empty, and Some otherwise.

        Now we can define a few basic functions that operate on lists and elements:

        let create () = ref None
        let is_empty t = !t = None

        let value elt = elt.value

        let first t = !t
        let next elt = elt.next
        let prev elt = elt.prev

        These all follow relatively straightforwardly from our type definitions.

        Cyclic Data Structures

        Doubly linked lists are a cyclic data structure, meaning that it is possible to follow a nontrivial sequence of pointers that closes in on itself. 
        In general, building cyclic data structures requires the use of side effects. 
        This is done by constructing the data elements first, and then adding cycles using assignment afterward.

        There is an exception to this, though: you can construct fixed-size cyclic data structures using let rec:

        This approach is quite limited, however. General-purpose cyclic data structures require mutation.

    Modifying the List

        Now, we'll start considering operations that mutate the list, starting with insert_first, which inserts an element at the front of the list:

        let insert_first t value =
          let new_elt = { prev = None; next = !t; value } in
          begin match !t with
          | Some old_first -> old_first.prev <- Some new_elt
          | None -> ()
          end;
          t := Some new_elt;
          new_elt

        insert_first first defines a new element new_elt, and then links it into the list, finally setting the list itself to point to new_elt. 
        Note that the precedence of a match expression is very low, so to separate it from the following assignment (t := Some new_elt), we surround the match with begin ... end. 
        We could have used parentheses for the same purpose. Without some kind of bracketing, the final assignment would incorrectly become part of the None case.

        We can use insert_after to insert elements later in the list. insert_after takes as arguments both an element after which to insert the new node and a value to insert:

        let insert_after elt value =
          let new_elt = { value; prev = Some elt; next = elt.next } in
          begin match elt.next with
          | Some old_next -> old_next.prev <- Some new_elt
          | None -> ()
          end;
          elt.next <- Some new_elt;
          new_elt

        Finally, we need a remove function:

          let remove t elt =
            let { prev; next; _ } = elt in
            begin match prev with
            | Some prev -> prev.next <- next
            | None -> t := next
            end;
            begin match next with
            | Some next -> next.prev <- prev;
            | None -> ()
            end;
            elt.prev <- None;
            elt.next <- None

        Note that the preceding code is careful to change the prev pointer of the following element and the next pointer of the previous element, if they exist. 
        If there's no previous element, then the list pointer itself is updated. In any case, the next and previous pointers of the element itself are set to None.

        These functions are more fragile than they may seem. In particular, misuse of the interface may lead to corrupted data. 
        For example, double-removing an element will cause the main list reference to be set to None, thus emptying the list. 
        Similar problems arise from removing an element from a list it doesn't belong to.

        This shouldn't be a big surprise. Complex imperative data structures can be quite tricky, considerably trickier than their pure equivalents. 
        The issues described previously can be dealt with by more careful error detection, 
        and such error correction is taken care of in modules like Core's Doubly_linked. 
        You should use imperative data structures from a well-designed library when you can. 
        And when you can't, you should make sure to put great care into your error handling.

    Iteration Functions

        When defining containers like lists, dictionaries, and trees, you'll typically want to define a set of iteration functions like iter, map, and fold, 
        which let you concisely express common iteration patterns.

        Dlist has two such iterators: iter, the goal of which is to call a unit-producing function on every element of the list, 
        in order; and find_el, which runs a provided test function on each values stored in the list, 
        returning the first element that passes the test. 
        Both iter and find_el are implemented using simple recursive loops 
        that use next to walk from element to element and value to extract the element from a given node:

        let iter t ~f =
          let rec loop = function
            | None -> ()
            | Some el -> f (value el); loop (next el)
          in
          loop !t

        let find_el t ~f =
          let rec loop = function
            | None -> None
            | Some elt ->
              if f (value elt) then Some elt
              else loop (next elt)
          in
          loop !t

        This completes our implementation, but there's still considerably more work to be done to make a really usable doubly linked list. 
        As mentioned earlier, you're probably better off using something like Core's Doubly_linked module 
        that has a more complete interface and has more of the tricky corner cases worked out. 
        Nonetheless, this example should serve to demonstrate some of the techniques you can use to build nontrivial imperative data structure in OCaml, 
        as well as some of the pitfalls.

    LAZINESS AND OTHER BENIGN EFFECTS

      There are many instances where you basically want to program in a pure style, 
      but you want to make limited use of side effects to improve the performance of your code. 
      Such side effects are sometimes called benign effects, 
      and they are a useful way of leveraging OCaml's imperative features while still maintaining most of the benefits of pure programming.

      One of the simplest benign effects is laziness. A lazy value is one that is not computed until it is actually needed. 
      In OCaml, lazy values are created using the lazy keyword, which can be used to convert any expression of type s into a lazy value of type s Lazy.t. 
      The evaluation of that expression is delayed until forced with Lazy.force:

      # let v = lazy (print_string "performing lazy computation\n"; sqrt 16.);;
       val v : float lazy_t = <lazy>
      # Lazy.force v;;

      performing lazy computation
       - : float = 4.
      # Lazy.force v;;
       - : float = 4.

      You can see from the print statement that the actual computation was performed only once, and only after force had been called.

      To better understand how laziness works, let's walk through the implementation of our own lazy type. 
      We'll start by declaring types to represent a lazy value:

      # type 'a lazy_state =
          | Delayed of (unit -> 'a)
          | Value of 'a
          | Exn of exn
        ;;
       type 'a lazy_state = Delayed of (unit -> 'a) | Value of 'a | Exn of exn

      A lazy_state represents the possible states of a lazy value. 
      A lazy value is Delayed before it has been run, where Delayed holds a function for computing the value in question. 
      A lazy value is in the Value state when it has been forced and the computation ended normally. 
      The Exn case is for when the lazy value has been forced, but the computation ended with an exception. 
      A lazy value is simply a ref containing a lazy_state, 
      where the ref makes it possible to change from being in the Delayed state to being in the Value or Exn states.

      We can create a lazy value from a thunk, i.e., a function that takes a unit argument. 
      Wrapping an expression in a thunk is another way to suspend the computation of an expression:

      # let create_lazy f = ref (Delayed f);;
       val create_lazy : (unit -> 'a) -> 'a lazy_state ref = <fun>
      # let v = create_lazy
          (fun () -> print_string "performing lazy computation\n"; sqrt 16.);;
       val v : float lazy_state ref = {contents = Delayed <fun>}

      Now we just need a way to force a lazy value. The following function does just that:

      # let force v =
          match !v with
          | Value x -> x
          | Exn e -> raise e
          | Delayed f ->
            try
              let x = f () in
              v := Value x;
              x
            with exn ->
              v := Exn exn;
              raise exn
           ;;
       val force : 'a lazy_state ref -> 'a = <fun>

      Which we can use in the same way we used Lazy.force:

      # force v;;


      performing lazy computation
      - : float = 4.
      # force v;;
      - : float = 4.

      The main user-visible difference between our implementation of laziness and the built-in version is syntax. 
      Rather than writing create_lazy (fun () -> sqrt 16.), we can (with the built-in lazy) just write lazy (sqrt 16.).

    Memoization and Dynamic Programming

      Another benign effect is memoization. A memoized function remembers the result of previous invocations of the function so that they can be returned without further computation when the same arguments are presented again.

      Here's a function that takes as an argument an arbitrary single-argument function and returns a memoized version of that function. Here we'll use Core's Hashtbl module, rather than our toy Dictionary:

      # let memoize f =
          let table = Hashtbl.Poly.create () in
          (fun x ->
            match Hashtbl.find table x with
            | Some y -> y
            | None ->
              let y = f x in
              Hashtbl.add_exn table ~key:x ~data:y;
              y
          );;
       val memoize : ('a -> 'b) -> 'a -> 'b = <fun>

      The preceding code is a bit tricky. memoize takes as its argument a function f and then allocates a hash table (called table) and returns a new function as the memoized version of f. 
      When called, this new function looks in table first, and if it fails to find a value, calls f and stashes the result in table. 
      Note that table doesn't go out of scope as long as the function returned by memoize is in scope.

      Memoization can be useful whenever you have a function that is expensive to recompute and you don't mind caching old values indefinitely. 
      One important caution: a memoized function by its nature leaks memory. 
      As long as you hold on to the memoized function, you're holding every result it has returned thus far.

      Memoization is also useful for efficiently implementing some recursive algorithms. 
      One good example is the algorithm for computing the edit distance (also called the Levenshtein distance) between two strings. 
      The edit distance is the number of single-character changes (including letter switches, insertions, and deletions) required to convert one string to the other. 
      This kind of distance metric can be useful for a variety of approximate string-matching problems, like spellcheckers.

      Consider the following code for computing the edit distance. Understanding the algorithm isn't important here, but you should pay attention to the structure of the recursive calls:

      # let rec edit_distance s t =
          match String.length s, String.length t with
          | (0,x) | (x,0) -> x
          | (len_s,len_t) ->
            let s' = String.drop_suffix s 1 in
            let t' = String.drop_suffix t 1 in
            let cost_to_drop_both =
              if s.[len_s - 1] = t.[len_t - 1] then 0 else 1
            in
            List.reduce_exn ~f:Int.min
              [ edit_distance s' t  + 1
              ; edit_distance s  t' + 1
              ; edit_distance s' t' + cost_to_drop_both
              ]
        ;;
       val edit_distance : string -> string -> int = <fun>
      # edit_distance "OCaml" "ocaml";;
       - : int = 2

      The thing to note is that if you call edit_distance "OCaml" "ocaml", then that will in turn dispatch the following calls:

      edit_distance "OCam" "ocaml"
      edit_distance "OCaml" "ocam"
      edit_distance "OCam" "ocam"

      And these calls will in turn dispatch other calls:

      edit_distance "OCam" "ocaml"
        edit_distance "OCa" "ocaml"
        edit_distance "OCam" "ocam"
        edit_distance "OCa" "ocam"
      edit_distance "OCaml" "ocam"
        edit_distance "OCam" "ocam"
        edit_distance "OCaml" "oca"
        edit_distance "OCam" "oca"
      edit_distance "OCam" "ocam"
        edit_distance "OCa" "ocam"
        edit_distance "OCam" "oca"
        edit_distance "OCa" "oca"

      As you can see, some of these calls are repeats. For example, there are two different calls to edit_distance "OCam" "oca". 
      The number of redundant calls grows exponentially with the size of the strings, meaning that our implementation of edit_distance is brutally slow for large strings. 
      We can see this by writing a small timing function:

      # let time f =
          let start = Time.now () in
          let x = f () in
          let stop = Time.now () in
          printf "Time: %s\n" (Time.Span.to_string (Time.diff stop start));
          x ;;
      val time : (unit -> 'a) -> 'a = <fun>

      And now we can use this to try out some examples:

      # time (fun () -> edit_distance "OCaml" "ocaml");;


      Time: 0.698805ms
      - : int = 2
      # time (fun () -> edit_distance "OCaml 4.01" "ocaml 4.01");;


      Time: 1.78467s
      - : int = 2

      Just those few extra characters made it thousands of times slower!

      Memoization would be a huge help here, but to fix the problem, we need to memoize the calls that edit_distance makes to itself. This technique is sometimes referred to as dynamic programming. 
      To see how to do this, let's step away from edit_distance and instead consider a much simpler example: computing the nth element of the Fibonacci sequence. 
      The Fibonacci sequence by definition starts out with two 1s, with every subsequent element being the sum of the previous two. The classic recursive definition of Fibonacci is as follows:

      # let rec fib i =
          if i <= 1 then 1 else fib (i - 1) + fib (i - 2);;
      val fib : int -> int = <fun>

      This is, however, exponentially slow, for the same reason that edit_distance was slow: we end up making many redundant calls to fib. It shows up quite dramatically in the performance:

      # time (fun () -> fib 20);;


      Time: 0.379086ms
      - : int = 10946
      # time (fun () -> fib 40);;


      Time: 4.61983s
      - : int = 165580141

      As you can see, fib 40 takes thousands of times longer to compute than fib 20.

      So, how can we use memoization to make this faster? The tricky bit is that we need to insert the memoization before the recursive calls within fib. 
      We can't just define fib in the ordinary way and memoize it after the fact and expect the first call to fib to be improved:

      # let fib = memoize fib;;
      val fib : int -> int = <fun>
      # time (fun () -> fib 40);;


      Time: 4.90749s
      - : int = 165580141
      # time (fun () -> fib 40);;


      Time: 0.00286102ms
      - : int = 165580141


