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