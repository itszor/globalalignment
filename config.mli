type 'a params =
  {
    ins : int;
    del : int;
    sub : 'a -> 'a -> int
  }

val default : 'a params
