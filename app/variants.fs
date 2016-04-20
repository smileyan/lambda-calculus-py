Chapter 6. Variants

    into
        Variant types are one of the most useful features of OCaml and also one of the most unusual. 
        They let you represent data that may take on multiple different forms, where each form is marked by an explicit tag. 
        As we'll see, when combined with pattern matching, variants give you a powerful way of representing complex data and of organizing the case-analysis on that information.

        The basic syntax of a variant type declaration is as follows:

            type <variant> =
              | <Tag> [ of <type> [* <type>]... ]
              | <Tag> [ of <type> [* <type>]... ]
              | ...

        Each row essentially represents a case of the variant. Each case has an associated tag and may optionally have a sequence of fields, where each field has a specified type.

        Let's consider a concrete example of how variants can be useful. Almost all terminals support a set of eight basic colors, 
        and we can represent those colors using a variant. 
        Each color is declared as a simple tag, with pipes used to separate the different cases. 
        Note that variant tags must be capitalized:

            # type basic_color =
               | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White ;;
             type basic_color =
                 Black
               | Red
               | Green
               | Yellow
               | Blue
               | Magenta
               | Cyan
               | White
            # Cyan ;;
             - : basic_color = Cyan

            # [Blue; Magenta; Red] ;;
             - : basic_color list = [Blue; Magenta; Red]

        The following function uses pattern matching to convert a basic_color to a corresponding integer. 
        The exhaustiveness checking on pattern matches means that the compiler will warn us if we miss a color:

            # let basic_color_to_int = function
              | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
              | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7 ;;
             val basic_color_to_int : basic_color -> int = <fun>
            # List.map ~f:basic_color_to_int [Blue;Red];;
             - : int list = [4; 1]
        Using the preceding function, we can generate escape codes to change the color of a given string displayed in a terminal:

            # let color_by_number number text =
                sprintf "\027[38;5;%dm%s\027[0m" number text;;
             val color_by_number : int -> string -> string = <fun>
            # let blue = color_by_number (basic_color_to_int Blue) "Blue";;
             val blue : string = "\027[38;5;4mBlue\027[0m"
            # printf "Hello %s World!\n" blue;;
             Hello Blue World!

        On most terminals, that word "Blue" will be rendered in blue.

        In this example, the cases of the variant are simple tags with no associated data. 
        This is substantively the same as the enumerations found in languages like C and Java. 
        But as we'll see, variants can do considerably more than represent a simple enumeration. 
        As it happens, an enumeration isn't enough to effectively describe the full set of colors that a modern terminal can display. 
        Many terminals, including the venerable xterm, support 256 different colors, broken up into the following groups:

            The eight basic colors, in regular and bold versions

            A 6 × 6 × 6 RGB color cube

            A 24-level grayscale ramp

        We'll also represent this more complicated color space as a variant, but this time, 
        the different tags will have arguments that describe the data available in each case. 
        Note that variants can have multiple arguments, which are separated by *s:

            # type weight = Regular | Bold
              type color =
              | Basic of basic_color * weight (* basic colors, regular and bold *)
              | RGB   of int * int * int       (* 6x6x6 color cube *)
              | Gray  of int                   (* 24 grayscale levels *)
             ;;
              type weight = Regular | Bold
              type color =
                  Basic of basic_color * weight
                | RGB of int * int * int
                | Gray of int
            # [RGB (250,70,70); Basic (Green, Regular)];;
             - : color list = [RGB (250, 70, 70); Basic (Green, Regular)]

        Once again, we'll use pattern matching to convert a color to a corresponding integer. 
        But in this case, the pattern matching does more than separate out the different cases; 
        it also allows us to extract the data associated with each tag:

            # let color_to_int = function
                | Basic (basic_color,weight) ->
                  let base = match weight with Bold -> 8 | Regular -> 0 in
                  base + basic_color_to_int basic_color
                | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
                | Gray i -> 232 + i ;;
             val color_to_int : color -> int = <fun>

        Now, we can print text using the full set of available colors:

            # let color_print color s =
                 printf "%s\n" (color_by_number (color_to_int color) s);;
             val color_print : color -> string -> unit = <fun>

            # color_print (Basic (Red,Bold)) "A bold red!";;
             A bold red!
            # color_print (Gray 4) "A muted gray...";;
             A muted gray...

    CATCH-ALL CASES AND REFACTORING
        OCaml's type system can act as a refactoring tool, warning you of places where your code needs to be updated to match an interface change. 
        This is particularly valuable in the context of variants.

        Consider what would happen if we were to change the definition of color to the following:

            # type color =
              | Basic of basic_color     (* basic colors *)
              | Bold  of basic_color     (* bold basic colors *)
              | RGB   of int * int * int (* 6x6x6 color cube *)
              | Gray  of int             (* 24 grayscale levels *)
             ;;
              type color =
                  Basic of basic_color
                | Bold of basic_color
                | RGB of int * int * int
                | Gray of int

        We've essentially broken out the Basic case into two cases, Basic and Bold, and Basic has changed from having two arguments to one. 
        color_to_int as we wrote it still expects the old structure of the variant, and 
        if we try to compile that same code again, the compiler will notice the discrepancy:

            # let color_to_int = function
                | Basic (basic_color,weight) ->
                  let base = match weight with Bold -> 8 | Regular -> 0 in
                  base + basic_color_to_int basic_color
                | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
                | Gray i -> 232 + i ;;
             Characters 34-60:
             Error: This pattern matches values of type 'a * 'b
                    but a pattern was expected which matches values of type basic_color

        Here, the compiler is complaining that the Basic tag is used with the wrong number of arguments. 
        If we fix that, however, the compiler flag will flag a second problem, which is that we haven't handled the new Bold tag:

            # let color_to_int = function
                | Basic basic_color -> basic_color_to_int basic_color
                | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
                | Gray i -> 232 + i ;;

             Characters 19-154:
             Warning 8: this pattern-matching is not exhaustive.
             Here is an example of a value that is not matched:
             Bold _val color_to_int : color -> int = <fun>

        Fixing this now leads us to the correct implementation:

            # let color_to_int = function
                | Basic basic_color -> basic_color_to_int basic_color
                | Bold  basic_color -> 8 + basic_color_to_int basic_color
                | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
                | Gray i -> 232 + i ;;
             val color_to_int : color -> int = <fun>

        As we've seen, the type errors identified the things that needed to be fixed to complete the refactoring of the code. 
        This is fantastically useful, but for it to work well and reliably, you need to write your code in a way that maximizes the compiler's chances of helping you find the bugs. 
        To this end, a useful rule of thumb is to avoid catch-all cases in pattern matches.

        Here's an example that illustrates how catch-all cases interact with exhaustion checks. 
        Imagine we wanted a version of color_to_int that works on older terminals by rendering the first 16 colors (the eight basic_colors in regular and bold) in the normal way, 
        but renders everything else as white. We might have written the function as follows:

            # let oldschool_color_to_int = function
                | Basic (basic_color,weight) ->
                  let base = match weight with Bold -> 8 | Regular -> 0 in
                  base + basic_color_to_int basic_color
                | _ -> basic_color_to_int White;;

             Characters 44-70:
             Error: This pattern matches values of type 'a * 'b
                    but a pattern was expected which matches values of type basic_color

        But because the catch-all case encompasses all possibilities, 
        the type system will no longer warn us that we have missed the new Bold case when we change the type to include it. 
        We can get this check back by avoiding the catch-all case, and instead being explicit about the tags that are ignored.



    Combining Records and Variants

        The term algebraic data types is often used to describe a collection of types that includes variants, records, and tuples. 
        Algebraic data types act as a peculiarly useful and powerful language for describing data. 
        At the heart of their utility is the fact that they combine two different kinds of types: product types, 
        like tuples and records, which combine multiple different types together and 
        are mathematically similar to Cartesian products; 
        and sum types, like variants, which let you combine multiple different possibilities into one type, and are mathematically similar to disjoint unions.

        Algebraic data types gain much of their power from the ability to construct layered combinations of sums and products. 
        Let's see what we can achieve with this by revisiting the logging server types that were described in Chapter 5, Records. 
        We'll start by reminding ourselves of the definition of Log_entry.t:

            # module Log_entry = struct
                type t =
                  { session_id: string;
                    time: Time.t;
                    important: bool;
                    message: string;
                  }
              end
              ;;
             module Log_entry :
               sig
                 type t = {
                   session_id : string;
                   time : Time.t;
                   important : bool;
                   message : string;
                 }
               end

        This record type combines multiple pieces of data into one value. 
        In particular, a single Log_entry.t has a session_id and a time and an important flag and a message. 
        More generally, you can think of record types as conjunctions. 
        Variants, on the other hand, are disjunctions, letting you represent multiple possibilities, as in the following example:

            # type client_message = | Logon of Logon.t
                                    | Heartbeat of Heartbeat.t
                                    | Log_entry of Log_entry.t
              ;;
             type client_message =
                 Logon of Logon.t
               | Heartbeat of Heartbeat.t
               | Log_entry of Log_entry.t


    Variants and Recursive Data Structures



    Polymorphic Variants



        Example: Terminal Colors Redux



        When to Use Polymorphic Variants



