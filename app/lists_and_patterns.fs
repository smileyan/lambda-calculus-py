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
     