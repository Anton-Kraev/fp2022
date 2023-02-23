Arithmetic
  $ ./demoInfer.exe << EOF
  > ((1 + 2) / 3 - 4) * ~5
  int

Tuple
  $ ./demoInfer.exe << EOF
  > fn x => ("some string", 'c', 1, true)
  'a -> string * char * int * bool

Id function
  $ ./demoInfer.exe << EOF
  > fn x => x
  'a -> 'a

Bypassing value restriction using eta-expansion
  $ ./demoInfer.exe << EOF
  > let val f = fn x y => let val id = (fn x => x) val idid = (fn x => id id x) in
  > (case idid x of true => 1 | _ => 0) + (case idid y of 1 => 1 | _ => 0) end
  > in f true 1 end
  int

Equality types
  $ ./demoInfer.exe << EOF
  > fn x y => x = y
  ''c -> ''c -> bool
  $ ./demoInfer.exe << EOF
  > fn first arr => case arr of [] => false | h::t => h = first
  ''f -> ''f list -> bool

All in one
  $ ./demoInfer.exe << EOF
  > fn x y f a b => ([x = y andalso (case a of [] => f b | h :: t => f h) = 0, true], "some")
  ''f -> ''f -> ('h -> int) -> 'h list -> 'h -> bool list * string

Errors
  $ ./demoInfer.exe << EOF
  > fn x y => x = y orelse y + x
  Unification failed: type of the expression is int but expected type was bool
  $ ./demoInfer.exe << EOF
  > fn x => y
  No such variable: y
  $ ./demoInfer.exe << EOF
  > let val f = fn x y => let val id = (fn x => x) val idid = (id id) in
  > (case idid x of true => 1 | _ => 0) + (case idid y of 1 => 1 | _ => 0) end
  > in f true 1 end
  Value restriction: type of idid cannot be generalized because its declaration is expansive (not a value). 
