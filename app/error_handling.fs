Chapter 7. Error Handling

    intro

        Nobody likes dealing with errors. It's tedious, it's easy to get wrong, and 
        it's usually just not as fun as planning out how your program is going to succeed. 
        But error handling is important, and however much you don't like thinking about it, 
        having your software fail due to poor error handling is worse.

        Thankfully, OCaml has powerful tools for handling errors reliably and with a minimum of pain. 
        In this chapter we'll discuss some of the different approaches in OCaml to handling errors, 
        and give some advice on how to design interfaces that make error handling easier.

        We'll start by describing the two basic approaches for reporting errors in OCaml: 
        error-aware return types and exceptions.

    ERROR-AWARE RETURN TYPES

        The best way in OCaml to signal an error is to include that error in your return value. Consider the type of the find function in the List module:

        # List.find;;
         - : 'a list -> f:('a -> bool) -> 'a option = <fun>

        The option in the return type indicates that the function may not succeed in finding a suitable element:

        # List.find [1;2;3] ~f:(fun x -> x >= 2) ;;
         - : int option = Some 2
        # List.find [1;2;3] ~f:(fun x -> x >= 10) ;;
         - : int option = None

        Including errors in the return values of your functions requires the caller to handle the error explicitly, 
        allowing the caller to make the choice of whether to recover from the error or propagate it onward.

        Consider the compute_bounds function. 
        The function takes a list and a comparison function and 
        returns upper and lower bounds for the list by finding the smallest and largest element on the list. 
        List.hd and List.last, which return None when they encounter an empty list, are used to extract the largest and smallest element of the list:

        # let compute_bounds ~cmp list =
            let sorted = List.sort ~cmp list in
            match List.hd sorted, List.last sorted with
            | None,_ | _, None -> None
            | Some x, Some y -> Some (x,y)
          ;;
          val compute_bounds : cmp:('a -> 'a -> int) -> 'a list -> ('a * 'a) option =
            <fun>

        The match statement is used to handle the error cases, propagating a None in hd or last into the return value of compute_bounds.

        On the other hand, in the find_mismatches that follows, errors encountered during the computation do not propagate to the return value of the function. 
        find_mismatches takes two hash tables as arguments and searches for keys that have different data in one table than in the other. 
        As such, the failure to find a key in one table isn't a failure of any sort:

        # let find_mismatches table1 table2 =
             Hashtbl.fold table1 ~init:[] ~f:(fun ~key ~data mismatches ->
                match Hashtbl.find table2 key with
                | Some data' when data' <> data -> key :: mismatches
                | _ -> mismatches
             )
          ;;
         val find_mismatches : ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> 'a list =
           <fun>
