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