let show_res alignment =
  List.iter
    (fun (q, _) ->
      match q with
        Some s -> Printf.printf "%s" s
      | None -> Printf.printf "-")
    alignment;
  print_newline ();
  List.iter
    (fun (_, r) ->
      match r with
        Some s -> Printf.printf "%s" s
      | None -> Printf.printf "-")
    alignment;
  print_newline ()

let _ =
  let arr1 = [| "a"; "b"; "c"; "d"; "e" |]
  and arr2 = [| "a"; "b"; "d"; "e" |] in
  show_res (Global_alignment.hirschberg arr1 arr2);
  print_newline ();
  show_res (Global_alignment.needleman_wunsch arr1 arr2);
  print_newline ();
  let arr1 = [| "1"; "2"; "3"; "4"; "5" |]
  and arr2 = [| "3" |] in
  show_res (Global_alignment.hirschberg arr1 arr2);
  print_newline ();
  show_res (Global_alignment.needleman_wunsch arr1 arr2);
  print_newline ();
  let arr1 = [| "x"; "1"; "2"; "3"; "2"; "1" |]
  and arr2 = [| "1"; "2"; "3"; "4"; "5"; "x"; "x" |]
  and cfg = { Global_alignment.Config.del = -2; ins = -2;
              sub = fun a b -> if a = b then 2 else -1 } in
  show_res (Global_alignment.hirschberg ~config:cfg arr1 arr2);
  print_newline ();
  show_res (Global_alignment.needleman_wunsch ~config:cfg arr1 arr2);
  print_newline ();
