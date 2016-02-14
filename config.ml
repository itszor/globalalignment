type 'a params =
  {
    ins : int;
    del : int;
    sub : 'a -> 'a -> int
  }

let default =
  {
    ins = -1;
    del = -1;
    sub = fun a b -> if a = b then 1 else -1
  }
