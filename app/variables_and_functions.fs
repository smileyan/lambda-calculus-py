VARIABLES

  let <variable> = <expr>
  
  # let x = 3;;
   val x : int = 3
  # let y = 4;;
   val y : int = 4
  # let z = x + y;;
   val z : int = 7
   
   let <variable> = <expr1> in <expr2>
  
  # let languages = "Ocaml,Perl,C++,C";;
   val languages : string = "Ocaml,Perl,C++,C"
  # let dashed_languages =
      let language_list = String.split languages ~on:',' in
      String.concat ~seq:"-" language_list
    ;;
   val dashed_languages : string = "Ocaml-Perl-C++-C"
   
  # language_list;;
   Characters -1-13:
   Error: Unbound value language_list
  
  # let languages = "Ocaml,Perl,C++,C";;
   val languages : string = "Ocaml,Perl,C++,C"
  # let dashed_languages =
      let languages = String.split languages ~on:',' in
      String.concat ~seq:"-" languages
    ;;
   val dashed_languages : string = "Ocaml-Perl-C++-C"

  # languages;;
   - : string = "Ocaml,Perl,C++,C"
 
  # let area_of_ring inner_radius outer_radius =
       let pi = acos (-1.) in
       let area_of_circle r = pi *. r *. r in
       area_of_circle outer_radius -. area_of_circle inner_radius
    ;;
   val area_of_ring : float -> float -> float = <fun>
  # area_of_ring 1. 3.;;
   - : float = 25.132
  
  Pattern Matching and let
  
  # let (ints,strings) = List.unzip [(1,"one"); (2,"two"); (3,"three")];;
   val ints : int list = [1; 2; 3]
   val strings : string list = ["one"; "two", "three"]
  
  # let upcase_first_entry line =
       let (first :: last) = String.split ~on:',' line in
       String.concat ~sep:"," (String.uppercase first :: rest)
    ;;
    
  Characters 40-53:
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a value that is not matched:
  []val upcase_first_entry : string -> string = <fun>
  
 # let upcase_first_entry line =
      match String.split ~on:',' line with
      | [] -> assert false (* String.split returns at least one element *)
      | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)
   ;;
  val upcase_first_entry : string -> string = <fun>
  
FUNCTIONS
 Anonymous Functions
 
  # (fun x -> x + 1);;
   - : int -> int = <fun>
 
  # (fun x -> x + 1) 7;;
   - : int = 8
 
  # List.map ~f:(fun x -> x + 1) [1;2;3];;
   - : int list = [2; 3; 4]
 
  # let increments = [ (fun x -> x + 1); (fun x -> x + 2) ];;
   val increments : (int -> int) list = [<fun>; <fun>]
  # List.map ~f:(fun g -> g 5) increments;;
   - : int list = [6; 7]
 
  # let plusone = (fun x -> x + 1);;
   val plusone : int -> int = <fun>
  # plusone 3;;
   - : int = 4
 
  # let plusone x = x + 1;;
   val plusone : int -> int = <fun>
  
  # (fun x -> x + 1) 7;;
   - : int = 8
  # let x = 7 in x + 1;;
   - : int = 8

 Multiargument functions

  # let abs_diff x y = abs (x - y);;
   val abs_diff : int -> int -> int = <fun>
  # abs_diff 3 4;;
   - : int = 1
 
  # let abs_diff = 
      (fun x -> (fun y -> abs (x - y)));;
   val abs_diff : int -> int -> int = <fun>
 
  # let dist_from_3 = abs_diff 3;;
   val dist_from_3 : int -> int = <fun>
  # dist_from_3 8;;
   - : int = 5
  # dist_from_3 (-1);;
   - : int = 4
 
  # let abs_diff = (fun x y -> abs (x -y));;
 
  # let abs_diff (x,y) = abs (x - y);;
   val abs_diff : int * int -> int = <fun>
  # abs_diff (3,4);;
   - : int = 1
 
 Recursive Functions
 
  # let rec find_first_stutter list =
      match list with
      | [] | [_] ->
        (* only zero or none element, so no repeats *)
        None
      | x :: y :: tl ->
        if x = y then Some x else find_first_stutter (y::tl)
     ;;
   val find_first_stutter : 'a list -> 'a list = <fun>
  
  # let rec is_even x =
      if x = 0 then true else is_odd (x - 1)
    and is_odd x =
      if x = 0 then false else is_even (x - 1)
   ;;
   val is_even : int -> bool = <fun>
   val is_odd : int -> bool = <fun>
  # List.map ~f:is_even [0;1;2;3;4;5];;
   - : bool list = [true; false; true; false; true; false]
   - : bool list = [false; true; false; true; false; true]
 
 Prefix and Infix Operators
 
  # Int.max 3 4  (* prefix *);;
   - : int = 4
  # 3 + 4        (* infix  *);;
   - : int = 7
  # (+) 3 4;;
   - : int = 7
  # List.map ~f:((+) 3) [4;5;6];;
   - : int list = [7; 8; 9]
  
  ! $ % & * + - . / : < = > ? @ ^ | ~
  
  # let (+!) (x1,y1) (x2,y2) = (x1 + x2, y1+ y2);;
   val ( +! ) : int * int -> int * int -> int * int = <fun>
  # (3,2) +! (-2,4);;
   - : int * int = (1, 6)
  
  # let (***) x y = (x ** y) ** y;;
   Characters 17-18:
   Error: This expression has type int but an expression was expected of type float
   
  # let ( *** ) x y = (x ** y) ** y;;
   val ( *** ) : float -> float -> float = <fun>
  
  # Int.max 3 (-4);;
   - : int = 3
  # Int.max 3 -4;;
   Characters -1-9:
   Error: This expression has type int -> int
          but an expression was expected of type int
  
  # (Int.max 3) -4;;
   Characters -1-10:
   Error: This epression has type int -> int
          but an expression was expected of type int
  
  # let (|>) x f = f x ;;
   val ( |> ) : 'a -> ('a -> b') -> 'a = <fun>
  
  # let path = "/usr/bin:/usr/local/bin:/bin:/sbin";;
   val path : string = "/usr/bin:/usr/local/bin:/bin:/sbin"
  #    String.split ~on:':' path
     |> List.dedup ~compare:String.compare
     |> List.iter ~f:print_endline
     ;;
    
   /bin
   /sbin
   /usr/bin
   /usr/local/bin
   - : unit = ()
  
  #    let split_path = String.split ~on:":" path in
     let deduped_path = List.dedup ~compare:String.compare split_path in
     List.iter ~f:print_endline deduped_path
     ;;
     
     
    /bin
    /sbin
    /usr/bin
    /usr/local/bin
    - : unit = ()
  
  # List.iter ~f:print_endline ["Two", "lines"];;
  
   Two
   lines
   - : unit = ()
  
  # List.iter ~f:print_endline;;
   - : string list -> unit = <fun>

 Declaring Functions with Function
  # let some_or_zero = function
       | Some x -> x
       | None -> 0
    ;;
   val some_or_zero : int option -> int = <fun>
  # List.map ~f:some_or_zero [Some 3; None; Some 4];;
   - : int list = [3; 0; 4]
  
  # let some_or_zero num_opt =
      match num_opt with
      | Some x -> x
      | None -> 0
    ;;
   val some_or_zero : int option -> int = <fun>
  
  # let some_or_default default = function
       | Some x -> x
       | None -> default
    ;;
   val some_or_default : 'a -> 'a option -> 'a = <fun>
  # some_or_default 3 (Some 5);;
   - : int = 5
  # List.map ~f:(some_or_default 100) [Some 3; None; Some 4];;
   - : int list = [3; 100; 4]
 Labeled Arguments
  # let ratio ~num ~denom = float num /. float denom;;
   val ratio : num:int -> denom:int -> float = <fun>
  # ratio ~num:3 ~denom:10;;
   - : float = 0.3
  # ratio ~denom:10 ~num:3;;
   - : float = 0.3
  
  label punning
   # let num = 3 in
   let denom = 4 in
   ratio ~num ~denom;;
    - : float = 0.75
  
   val create_hashtable : int -> bool -> ('a,'b) Hashtable.t 
   val create_hashtable :
     init_size:int -> allow_shrinking:bool -> ('a, 'b) Hashtable.t
  
   val substring: string -> int -> int -> string
  
   val substring: string -> pos:int -> len:int -> string
  
   #    String.split ~on:':' path
     |> List.dedup ~compare:String.compare
     |> List.iter ~f:print_endline
     ;;
    
   /bin
   /sbin
   /usr/bin
   /usr/local/bin
   - : unit = ()
  
  Higher-order functions and labels
  
   # let apply_to_tuple f (first,second) = f ~first ~second;;
    val apply_to_tuple : (first:'a -> second:'b -> 'c) -> 'a * 'b -> 'c = <fun>
  
   # let apply_to_tuple_2 f (first,second) = f ~second ~first;;
    val apply_to_tuple_2 : (second:'a -> first:'b -> 'c) -> 'b * 'a -> 'c = <fun>
  
   # let divide ~first ~second = first / second;;
    val divide : first:int -> second:int -> int = <fun>
  
   # apply_to_tuple_2 divide (3,4);;
    Characters 17-23:
    Error: This expression has type first:int -> second:int -> int
           but an expression was expected of type second:'a -> first:'b -> 'c
   
   # apply_to_tuple divide (3,4);;
    - : int = 0

 Optional Arguments
  Induction
   # let concat ?seq x y =
     let seq = match seq with None -> "" | Some x -> x in
     x ^ seq ^ y
     ;;
    val concat : ?seq:string -> string -> string -> string = <fun>
   # concat "foo" "bar"     (* without the optional argument *);;
    - : string = "foobar"
   # concat ~seq:":" "foo" "bar"   (* with the optional argument *);;
    - : string = "foo:bar"
   
   # let concat ?(seq="") x y = x ^ seq ^ y;;
    val concat : ?seq:string -> string -> string -> string = <fun>
   
  Explicit passing of an optional argument
   # concat ~seq:":" "foo" "bar" (* provide the optional argument *);;
    - : string = "foo:bar"
   # concat ?seq:(Some ":") "foo" "bar" (* pass an explicit [Some] *);;
    - : string = "foo:bar"
   
   # concat "foo" "bar" (* don't provide the optional argument *);;
    - : string = "foobar"
   # concat ?seq:None "foo" "bar" (* explicitly pass `None` *);;
    - : string = "foobar"
   
   # let uppercase_concat ?(seq="") a b = concat ~seq (String.uppercase a) b ;;
    val uppercase_concat : ?seq:string -> string -> string -> string = <fun>
   # uppercase_concat "foo" "bar";;
    - : string = "FOObar"
   # uppercase_concat "foo" "bar" ~seq:":";;
    - : string = "FOO:bar"
   
   # let uppercase_concat ?seq a b = concat ?seq (String.uppercase a) b;;
    val uppercase_concat : ?seq:string -> string -> string -> string = <fun>
   
  Inference of labeled and optional arguments
   # let numeric_deriv ~delta ~x ~y ~f =
       let x' = x +. delta in
       let y' = y +. delta in
       let base = f ~x ~y in
       let dx = (f ~x:x' ~y -. base) /. delta in
       let dy = (f ~x ~y:y' -. base) /. delta in
       (dx, dy)
     ;;
    val numeric_deriv :
      delta:float ->
      x:float -> y:float -> f:(x:float -> y:float -> float) -> float * float = <fun>
   
   # let numeric_deriv ~delta ~x ~y ~f =
       let x' = x +. delta in
       let y' = y +. delta in
       let base = f ~x ~y in
       let dx = (f ~y ~x:x' -. base) /. delta in
       let dy = (f ~x ~y:y' -. base) /. delta in
       (dx, dy)
     ;; 
    Characters 130-131:
    Error: This function is applied to arguments
    in an order different from other calls.
    This is only allowed when the real type is known.
    
   # let numeric_deriv ~delta ~x ~y ~(f: x:float -> y:float -> float) =
      let x' = x +. delta in
      let y' = y +. delta in
      let base = f ~x ~y in
      let dx = (f ~y ~x:x' -. base) /. delta in
      let dy = (f ~x ~y:y' -. base) /. delta in
      (dx, dy)
    ;; 
    val numeric_deriv :
     delta:float ->
     x:float -> y:float -> f:(x:float -> y:float -> float) -> float * float = <fun>
  
  Optional arguments and partial application
   # let colon_concat = concat ~seq:":";;
    val colon_concat : string -> string -> string = <fun>
   # colon_concat "a" "b"
    - : string = "a:b"
   
   # let prepend_pound = concat "# ";;
    val prepend_pound : string -> string = <fun>
   # prepend_pound "a BASH  comment";;
    - : string = "# a BASH comment"
   
   # prepend_pound "a BASH comment" ~seq:":";;
    Characters -1-13:
    Error: This function has type string -> string
           It is applied to too many arguments; maybe you forgot a ';'.

   The rule is: an optional argument is erased as soon as the first positional 
    (i.e., neither labeled nor optional) argument defined after the optional argument is passed in.
   
   # let concat x ?(seq="") y = x ^ seq ^ y ;;
    val concat : string -> ?seq:string -> string -> string = <fun>
   # let prepend_pound = concat "# ";;
    val prepend_pound : ?seq:string -> string -> string = <fun>
   # prepend_pound "a BASH comment";;
    - : string = "# a BASH comment"
   # prepend_pound "a BASH comment" ~seq:"--- ";;
    - : string = "# --- a BASH comment"
   # concat "a" "b" ~seq:"=";;
    - : string = "a=b"
   # let concat x y ?(seq="") = x ^ seq ^ y ;;
   
    Characters 15-38
    Warning 16: this optional argument cannot be erased.val concat : string -> string -> ?seq:string -> string = <fun>
   # concat "a" "b";;
    - : ?seq:string -> string = <fun>