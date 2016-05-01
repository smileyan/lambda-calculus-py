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



