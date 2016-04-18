Chapter 6. Variants

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

