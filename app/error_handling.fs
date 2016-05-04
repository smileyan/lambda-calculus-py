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

        The use of options to encode errors underlines the fact that it's not clear whether a particular outcome, 
        like not finding something on a list, is an error or is just another valid outcome. 
        This depends on the larger context of your program, and thus is not something that a general-purpose library can know in advance. 
        One of the advantages of error-aware return types is that they work well in both situations.

    Encoding Errors with Result

        Options aren't always a sufficiently expressive way to report errors. 
        Specifically, when you encode an error as None, there's nowhere to say anything about the nature of the error.

        Result.t is meant to address this deficiency. The type is defined as follows:

        module Result : sig
           type ('a,'b) t = | Ok of 'a
                            | Error of 'b
        end

        A Result.t is essentially an option augmented with the ability to store other information in the error case. 
        Like Some and None for options, the constructors Ok and Error are promoted to the toplevel by Core.Std. As such, we can write:

        # [ Ok 3; Error "abject failure"; Ok 4 ];;
         - : (int, string) Result.t list = [Ok 3; Error "abject failure"; Ok 4]

        without first opening the Result module.

    Error and Or_error

        Result.t gives you complete freedom to choose the type of value you use to represent errors, 
        but it's often useful to standardize on an error type. Among other things, this makes it easier to write utility functions to automate common error handling patterns.

        But which type to choose? Is it better to represent errors as strings? Some more structured representation like XML? Or something else entirely?

        Core's answer to this question is the Error.t type, which tries to forge a good compromise between efficiency, convenience, and control over the presentation of errors.

        It might not be obvious at first why efficiency is an issue at all. 
        But generating error messages is an expensive business. 
        An ASCII representation of a value can be quite time-consuming to construct, particularly if it includes expensive-to-convert numerical data.

        Error gets around this issue through laziness. 
        In particular, an Error.t allows you to put off generation of the error string until and unless you need it, 
        which means a lot of the time you never have to construct it at all. You can of course construct an error directly from a string:

        # Error.of_string "something went wrong";;
         - : Error.t = something went wrong

        But you can also construct an Error.t from a thunk, i.e., a function that takes a single argument of type unit:

        # Error.of_thunk (fun () ->
            sprintf "something went wrong: %f" 32.3343);;
         - : Error.t = something went wrong: 32.334300

        In this case, we can benefit from the laziness of Error, since the thunk won't be called unless the Error.t is converted to a string.

        The most common way to create Error.ts is using s-expressions. An s-expression is a balanced parenthetical expression where the leaves of the expressions are strings. Here's a simple example:

        (This (is an) (s expression))

        S-expressions are supported by the Sexplib package that is distributed with Core and is the most common serialization format used in Core. 
        Indeed, most types in Core come with built-in s-expression converters. Here's an example of creating an error using the sexp converter for times, Time.sexp_of_t:

        # Error.create "Something failed a long time ago" Time.epoch Time.sexp_of_t;;
         - : Error.t =
         Something failed a long time ago: (1969-12-31 19:00:00.000000-05:00)

        Note that the time isn't actually serialized into an s-expression until the error is printed out.

        We're not restricted to doing this kind of error reporting with built-in types. 
        This will be discussed in more detail in Chapter 17, Data Serialization with S-Expressions, 
        but Sexplib comes with a language extension that can autogenerate sexp converters for newly generated types:

        # let custom_to_sexp = <:sexp_of<float * string list * int>>;;
         val custom_to_sexp : float * string list * int -> Sexp.t = <fun>
        # custom_to_sexp (3.5, ["a";"b";"c"], 6034);;
         - : Sexp.t = (3.5 (a b c) 6034)

        We can use this same idiom for generating an error:

        # Error.create "Something went terribly wrong"
            (3.5, ["a";"b";"c"], 6034)
            <:sexp_of<float * string list * int>> ;;
        - : Error.t = Something went terribly wrong: (3.5(a b c)6034)


        