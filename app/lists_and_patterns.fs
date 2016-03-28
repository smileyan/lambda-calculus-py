Chapter 3. Lists and Patterns
    LIST BASICS
        # [1;2;3] ;;
         - : int list = [1; 2; 3]
        # 1 :: (2 :: (3 :: [])) ;;
         - : int list = [1; 2; 3]
        # 1 :: 2 :: 3 :: [] ;;
         - : int list = [1; 2; 3]
        
        # let empty = [];;
         val empty : 'a list = []
        # 3 :: empty;;
         - : int list = [3]
        # "three" :: empty
         - : string list = ["three"]
         
         +---+---+   +---+---+   +---+---+
         | 1 | *---->| 2 | *---->| 3 | *---->||    singly linked lists.
         +---+---+   +---+---+   +---+---+
        
        # let l = 1 :: 2 :: 3 :: [];;
         val l : int list = [1; 2; 3]
        # let m = 0 :: l;;
         val m : int list = [0; 1; 2; 3]
        # l;;
         - : int list = [1; 2; 3]
    USING PATTERNS TO EXTRACT DATA FROM A LIST
        # let rec sum l =
             match l with
             | [] -> 0
             | h :: t -> h + ( sum t )
           ;;
          val sum : int list -> int = <fun>
        # sum [1; 2; 3];;
         - : int = 6
        # sum [];;
         - : int = 0

        # let rec drop_value l to_drop =
            match l with
            | [] -> []
            | to_drop :: tl -> drop_value tl to_drop
            | hd :: tl -> hd :: drop_value tl to_drop
          ;;
         
         Characters 114-122:
         Warning 11: this match case is unused.val drop_value: 'a list -> 'a -> 'a list = <fun>

         # drop_value [1;2;3] 2
          - : int list = []
         
         # let rec drop_value l to_drop =
             match l with
             | [] -> []
             | hd :: tl ->
             let new_tl = drop_value tl to_drop in
             if hd = to_drop then new_tl else hd :: new_tl
           ;;
          val drop_value : 'a list -> 'a -> 'a list = <fun>
         # drop_value [1;2;3] 2;;
          - : int list = [1;3]
         # drop_zero l =
             match l with
             |       [] -> []
             | 0  :: tl -> drop_zero tl
             | hd :: tl -> hd :: drop_zero tl
           ;;
          val drop_zero : int list -> int list = <fun>
         # drop_zero [1;2;0;3];;
          - : int list = [1; 2; 3]
    LIMITATIONS (AND BLESSINGS) OF PATTERN MATCHING
        Performance
            opam install core_bench
            # let plus_one_match x =
                match x with
                | 0 -> 1
                | 1 -> 2
                | 2 -> 3
                | _ -> x + 1

              let plus_one_if x =
                if      x = 0 then 1
                else if x = 1 then 2
                else if x = 2 then 3
                else x + 1
              ;;
             val plus_one_match : int -> int = <fun>
             val plus_one_if : int -> int = <fun>
            # #require "core_bench";;
            
            # open Core_bench.Std;;
            
            # let run_bench tests =
              Bench.bench tests
             ;;
             val run_bench : Bench.Test.t list -> unit = <fun>
            # [ Bench.Test.create ~name:"plus_one_match" (fun () ->
                  ignore (plus_one_match 10))
              ; Bench.Test.create ~name:"plus_one_if" (fun () ->
                  ignore (plus_one_if 10)) ]
              |> run_bench
              ;;
             
             Estimated testing time 20s (2 benchmarks x 10s). Change using -quota SECS.
                ┌────────────────┬──────────┐
                │ Name           │ Time/Run │
                ├────────────────┼──────────┤
                │ plus_one_match │  25.69ns │
                │ plus_one_if    │  34.06ns │
                └────────────────┴──────────┘
             - : unit = ()
            # let rec sum_if l =
                if List.is_empty l then 0
                else List.hd_exn l + sum_if (List.tl_exn l)
              ;;
             val sum_if : int Core.Std.List.t -> int = <fun>
            
            # let numbers = List.range 0 1000 in
              [ Bench.Test.create ~name:"sum" (fun () -> ignore (sum numbers))
              ; Bench.Test.create ~name:"sum_if" (fun () -> ignore (sum_if numbers)) ]
              |> run_bench
              ;;
                Estimated testing time 20s (2 benchmarks x 10s). Change using -quota SECS.
                ┌────────┬──────────┐
                │ Name   │ Time/Run │
                ├────────┼──────────┤
                │ sum    │  11.51us │
                │ sum_if │  62.27us │
                └────────┴──────────┘
              - : unit = ()
        Detecting Errors
            # let drop_zero l =
                match l with
                | [] -> []
                | 0 :: tl -> drop_zero tl
              ;;
             Characters 26-84:
             Warning 8: this pattern-matching is not exhaustive.
             Here is an example of a value that is not matched:
             1::_val drop_zero : int list -> 'a list = <fun>
    USING THE LIST MODULE EFFECTIVELY
        # printf "%s\n"
            (render_table
              ["language";"architect";"first release"]
              [ ["Lisp" ;"John McCarthy" ;"1958"] ;
                ["C"    ;"Dennis Ritchie";"1969"] ;
                ["ML"   ;"Robin Milner"  ;"1973"] ;
                ["OCaml";"Xavier Leroy"  ;"1996"] ;
              ]);;


            | language | architect      | first release |
            |----------+----------------+---------------|
            | Lisp     | John McCarthy  | 1958          |
            | C        | Dennis Ritchie | 1969          |
            | ML       | Robin Milner   | 1973          |
            | OCaml    | Xavier Leroy   | 1996          |
            - : unit = ()
        # List.map ~f:String.length ["Hello"; "World!"];;
         - : int list = [5; 6]
        # List.map2_exn ~f:Int.max [1;2;3] [3;2;1];;
         - : int list = [3; 2; 3]
        # List.map2_exn ~f:Int.max [1;2;3] [3;2;1;0];;
         Exception: (Invalid_argument "length mismatch in rev_map2_exn: 3 <> 4 ").
        # List.fold;;
         - : 'a list -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum = <fun>
        # List.fold [1;2;3;4] ~init:0 ~f:(+);;
         - : int = 10
        # List.fold ~init:[] ~f:(fun list x -> x :: list) [1;2;3;4];;
         - : int list = [4;3;2;1]
        # let max_lenght header rows =
            let lengths l = List.map ~f:String.length l in
            List.fold rows
              ~init:(lengths header)
              ~f:(fun acc row ->
                List.map2_exn -f:Int.max acc (lengths row))
        # let render_separator widths =
            let pieces = List.map width 
              ~f:(fun w -> String.make (w + 2) '-')
            in
            "|" ^ String.concat ~seq:"+" pieces ^ "|"
          ;;
         val render_separator : int list -> string = <fun>
        # render_separator [3;6;2];;
         - : string = "|-----+--------+----|"
        # let pad s length =
            " " ^ s ^ String.make (length - String.length s + 1) " "
          ;;
         val pad : string -> int -> string = <fun>
        # pad "hello" 10;;
         - : string = " hello      "
        # let render_row row widths =
            let padded = List.map2_exn row widths ~f:pad in
            "|" ^ String.concat ~seq:"|" padded ^ "|"
          ;;
         val render_row : string list -> int list -> string = <fun>
        # let render_table headers rows =
            let widths = max_lenght headers rows in
            String.concat ~seq:"\n"
              (render_row headers widths
               :: render_separator widths
               :: List.map rows ~f:(fun row -> render_row row widths)
              )
          ;;
         val render_table : string list -> string list list -> string = <fun>
        More Useful List Functions
            Combining list elements with List.reduce
                # List.reduce
                 - : 'a list -> f:('a -> 'a -> 'a) -> 'a option = <fun>
                # List.reduce ~f:(+) [1;2;3;4];;
                 - : int option = Some 10
                # List.reduce ~f:(+) [];;
                 - : int option = None
            Filtering with List.filter and List.filter_map
                # List.filter ~f:(x -> x mod 2 = 0) [1;2;3;4;5];;
                 - : int list = [2; 4]
                # List.filter_map (Sys.dir ".") ~f:(fname ->
                    match String.rsplit2 ~on:"." fname with
                    | None | Some ("",_)  -> None
                    | Some (_,ext) -> Some ext)
                  |> String.dedup
                  ;;
                 - : string list = ["ascii"; "ml"; "mli"; "topscript"]
            Partitioning with List.partition_tf