// Chapter 4. Files, Modules, and Programs
    // SINGLE-FILE PROGRAMS
        # let assoc = [("one", 1); ("two",2); ("three",3)];;
        val assoc : (string * int) list = [("one", 1); ("two", 2); ("three", 3)]
        # List.Assoc.find assoc "two" ;;
        - : int option = Some 2
        # List.Assoc.add assoc "four" 4 (* add a new key*) ;;
        - : (string, int) List.Assoc.t =
        [("four", 4); ("one", 1); ("two", 2); ("three", 3)]
        # # List.Assoc.add assoc "two"  4 (* overwrite an existing key *) ;;
        - : (string, int) List.Assoc.t = [("two", 4); ("one", 1); ("three", 3)]
    // MULTIFILE PROGRAMS AND MODULES