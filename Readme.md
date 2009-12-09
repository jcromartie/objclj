ObjClj
======

ObjClj (rhymes with "hodgepodge") probably needs a new name, but it's kind of fun to say.

The goal of this project is to emit C code from Clojure, enabling the use
of more powerful macros and code generation than the C preprocessor alone.

Example:

    (c
      (include stdio)
      (defn void greet
        [char* name]
        (printf "Hello, %s\n" name))
      (defn int main
        [int argc, char** argv]
        (if (< 1 argc)
          (let [char* name (nth argv 1)]
            (greet name))
          (printf "What's your name?\n"))
        (return 0)))

Should (but currently does not, sorry) yield:

    #include <stdio.h>
    
    void greet(char* name) {
      printf("Hello, %s\n", name);
    }
    
    int main(int argc, char** argv) {
      if (1 < argc) {
        char* name = (argv)[1];
        greet(name);
      } else {
        printf("What's your name?\n");
      }
      return 0;
    }
    