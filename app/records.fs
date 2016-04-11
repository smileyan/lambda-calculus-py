Chapter 5. Records
    intro
        One of OCaml's best features is its concise and expressive system for declaring new data types, 
        and records are a key element of that system. We discussed records briefly in Chapter 1, 
        A Guided Tour, but this chapter will go into more depth, covering the details of how records work, 
        as well as advice on how to use them effectively in your software designs.

        A record represents a collection of values stored together as one, 
        where each component is identified by a different field name. 
        The basic syntax for a record type declaration is as follows:
            type <record-name> =
              { <field> : <type> ;
                <field> : <type> ;
                ...
              }
        Note that record field names must start with a lowercase letter.

        Here's a simple example, a host_info record that summarizes information about a given computer:

            # type host_info =
                { hostname   : string;
                  os_name    : string;
                  cpu_arch   : string;
                  timestamp  : Time.t;
                };;
             type host_info = {
               hostname : string;
               os_name : string;
               cpu_arch : string;
               timestamp : Time.t;
             }

        We can construct a host_info just as easily. The following code uses the Shell module from Core_extended to 
        dispatch commands to the shell to extract the information we need about the computer we're running on. 
        It also uses the Time.now call from Core's Time module:

            # #require "core_extended";;

            # open Core_extended.Std;;

            # let my_host =
                let sh = Shell.sh_one_exn in
                { hostname   = sh "hostname";
                  os_name    = sh "uname -s";
                  cpu_arch   = sh "uname -p";
                  timestamp  = Time.now ();
                };;
             val my_host : host_info =
               {hostname = "flick.local"; os_name = "Darwin"; cpu_arch = "i386";
                timestamp = 2013-11-05 08:49:38.850439-05:00}

        You might wonder how the compiler inferred that my_host is of type host_info. 
        The hook that the compiler uses in this case to figure out the type is the record field name. 
        Later in the chapter, we'll talk about what happens when 
        there is more than one record type in scope with the same field name.

        Once we have a record value in hand, we can extract elements from the record field using dot notation:

            # my_host.cpu_arch;;
             - : string = "i386"

        When declaring an OCaml type, you always have the option of parameterizing it by a polymorphic type.
        Records are no different in this regard. So, for example, here's a type one might use to timestamp arbitrary items:

            # type 'a timestamped = { item: 'a; time: Time.t };;
             type 'a timestamped = { item : 'a; time : Time.t; }

        We can then write polymorphic functions that operate over this parameterized type:

            # let first_timestamped list =
                List.reduce list ~f:(fun a b -> if a.time < b.time then a else b)
              ;;
             val first_timestamped : 'a timestamped list -> 'a timestamped option = <fun>

    PATTERNS AND EXHAUSTIVENESS

        Another way of getting information out of a record is by using a pattern match, 
        as in the definition of host_info_to_string:

        # let host_info_to_string { hostname = h; os_name = os;
                                    cpu_arch = c; timestamp = ts;
                                  } =
               sprintf "%s (%s / %s, on %s)" h os c (Time.to_sec_string ts);;
         val host_info_to_string : host_info -> string = <fun>
        # host_info_to_string my_host;;
         - : string = "flick.local (Darwin / i386, on 2013-11-05 08:49:38)"

        Note that the pattern we used had only a single case, rather than using several cases separated by |'s. 
        We needed only one pattern because record patterns are irrefutable, 
        meaning that a record pattern match will never fail at runtime. 
        This makes sense, because the set of fields available in a record is always the same. 
        In general, patterns for types with a fixed structure, like records and tuples, are irrefutable, 
        unlike types with variable structures like lists and variants.

        Another important characteristic of record patterns is that they don't need to be complete; 
        a pattern can mention only a subset of the fields in the record. 
        This can be convenient, but it can also be error prone. 
        In particular, this means that when new fields are added to the record, 
        code that should be updated to react to the presence of those new fields will not be flagged by the compiler.

        As an example, imagine that we wanted to add a new field to our host_info record called os_release:

            # type host_info =
                { hostname   : string;
                  os_name    : string;
                  cpu_arch   : string;
                  os_release : string;
                  timestamp  : Time.t;
                } ;;
             type host_info = {
               hostname : string;
               os_name : string;
               cpu_arch : string;
               os_release : string;
               timestamp : Time.t;
             }

        The code for host_info_to_string would continue to compile without change. In this particular case, 
        it's pretty clear that you might want to update host_info_to_string in order to include os_release, 
        and it would be nice if the type system would give you a warning about the change.

        Happily, OCaml does offer an optional warning for missing fields in a record pattern. 
        With that warning turned on (which you can do in the toplevel by typing #warnings "+9"), the compiler will warn about the missing field:

            # #warnings "+9";;
 
            # let host_info_to_string { hostname = h; os_name = os;
                                        cpu_arch = c; timestamp = ts;
                                      } =
                sprintf "%s (%s / %s, on %s)" h os c (Time.to_sec_string ts);;


             Characters 24-139:
             Warning 9: the following labels are not bound in this record pattern:
             os_release
             Either bind these labels explicitly or add '; _' to the pattern.val host_info_to_string : host_info -> string = <fun>

        We can disable the warning for a given pattern by explicitly acknowledging that we are ignoring extra fields. This is done by adding an underscore to the pattern:

        # let host_info_to_string { hostname = h; os_name = os;
                                    cpu_arch = c; timestamp = ts; _
                                  } =
            sprintf "%s (%s / %s, on %s)" h os c (Time.to_sec_string ts);;
         val host_info_to_string : host_info -> string = <fun>

            Compiler Warnings
                The OCaml compiler is packed full of useful warnings that can be enabled and disabled separately. 
                These are documented in the compiler itself, so we could have found out about warning 9 as follows:

                $ ocaml -warn-help | egrep '\b9\b'
                    9 Missing fields in a record pattern.
                    R Synonym for warning 9.

        You should think of OCaml's warnings as a powerful set of optional static analysis tools, 
        and you should eagerly enable them in your build environment. You don't typically enable all warnings, 
        but the defaults that ship with the compiler are pretty good.

        The warnings used for building the examples in this book are specified with the following flag: -w @A-4-33-41-42-43-34-44.

        The syntax of this can be found by running ocaml -help, but this particular invocation turns on all warnings as errors, 
        disabling only the numbers listed explicitly after the A.

        Treating warnings as errors (i.e., making OCaml fail to compile any code that triggers a warning) is good practice, 
        since without it, warnings are too often ignored during development. When preparing a package for distribution, however, 
        this is a bad idea, since the list of warnings may grow from one release of the compiler to another, 
        and so this may lead your package to fail to compile on newer compiler releases.





