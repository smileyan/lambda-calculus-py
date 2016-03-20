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