(use 'objclj.core)

(println (c
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
	    (return 0))))
