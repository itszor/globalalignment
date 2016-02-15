let result_string select alignment =
  let buf = Buffer.create 10 in
  List.iter
    (fun ent ->
      match select ent with
        Some s -> Buffer.add_string buf s
      | None -> Buffer.add_char buf '-')
    alignment;
  Buffer.contents buf

let show_res algo alignment lhs_exp rhs_exp =
  Printf.printf "Algorithm: %s\n" algo;
  let lhs = result_string fst alignment
  and rhs = result_string snd alignment in
  Printf.printf "%s\n%s\n\n" lhs rhs;
  if lhs <> lhs_exp then begin
    Printf.fprintf stderr "LHS doesn't match: '%s' vs '%s'\n" lhs lhs_exp;
    exit 1
  end else if rhs <> rhs_exp then begin
    Printf.fprintf stderr "RHS doesn't match: '%s' vs '%s'\n" rhs rhs_exp;
    exit 1
  end

let alt_config = { Global_alignment.Config.del = -2; ins = -2;
                   sub = fun a b -> if a = b then 2 else -1 }

let (inputs : (string array * string array *
               string Global_alignment.Config.params * string * string) list) =
[
  [| "a"; "b"; "c"; "d"; "e" |],
  [| "a"; "b"; "d"; "e" |],
  Global_alignment.Config.default,
  "abcde", "ab-de";

  [| "1"; "2"; "3"; "4"; "5" |],
  [| "3" |],
  Global_alignment.Config.default,
  "12345", "--3--";

  [| "x"; "1"; "2"; "3"; "2"; "1" |],
  [| "1"; "2"; "3"; "4"; "5"; "x"; "x" |],
  alt_config,
  "x123--21", "-12345xx";

  [| "A"; "G"; "T"; "A"; "C"; "G"; "C"; "A" |],
  [| "T"; "A"; "T"; "G"; "C" |],
  alt_config,
  "AGTACGCA", "--TATGC-"
]

let _ =
  ignore (List.fold_left
           (fun num (arr1, arr2, cfg, e1, e2) ->
             Printf.printf "== Test %d ==\n\n" num;
             show_res "hirschberg" (Global_alignment.hirschberg arr1 arr2)
                      e1 e2;
             show_res "needleman-wunsch"
                      (Global_alignment.needleman_wunsch arr1 arr2) e1 e2;
             succ num)
           1
           inputs)
