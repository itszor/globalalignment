module Config = struct
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
end

let needleman_wunsch ?(config=Config.default) arr1 arr2 =
  let open Config in
  let len1 = Array.length arr1
  and len2 = Array.length arr2 in
  let costs = Bigarray.(Array2.create int c_layout (len1 + 1) (len2 + 1)) in
  costs.{0, 0} <- 0;
  (* Fill in 1st row.  *)
  for i = 1 to len1 do
    costs.{i, 0} <- i * config.del;
  done;
  (* Fill in 1st column.  *)
  for j = 1 to len2 do
    costs.{0, j} <- j * config.ins;
  done;
  for j = 1 to len2 do
    for i = 1 to len1 do
      let elem1 = arr1.(i - 1)
      and elem2 = arr2.(j - 1) in
      let ul_cost = costs.{i - 1, j - 1} in
      let diag = ul_cost + config.sub elem1 elem2
      and left = costs.{i - 1, j} + config.ins
      and top = costs.{i, j - 1} + config.del in
      costs.{i, j} <- max diag (max left top)
    done
  done;
  (*for j = 0 to len2 do
    for i = 0 to len1 do
      Printf.printf "%4d " costs.{i, j}
    done;
    Printf.printf "\n"
  done;*)
  let rec walk m n steps =
    if m >= 1 && n >= 1 then begin
      let left = costs.{m - 1, n}
      and top = costs.{m, n - 1}
      and diag = costs.{m - 1, n - 1} in
      let elem1 = arr1.(m - 1)
      and elem2 = arr2.(n - 1) in
      if diag >= top && diag >= left then
        walk (m - 1) (n - 1) ((Some elem1, Some elem2) :: steps)
      else if top > left then
        walk m (n - 1) ((None, Some elem2) :: steps)
      else
        walk (m - 1) n ((Some elem1, None) :: steps)
    end else if m >= 1 then begin
      (* On the top row.  *)
      let elem1 = arr1.(m - 1) in
      walk (m - 1) n ((Some elem1, None) :: steps)
    end else if n >= 1 then begin
      (* On the left column.  *)
      let elem2 = arr2.(n - 1) in
      walk m (n - 1) ((None, Some elem2) :: steps)
    end else
      steps in
  walk len1 len2 []

let nwscore ~config arr1 arr2 =
  let open Config in
  let len1 = Array.length arr1
  and len2 = Array.length arr2 in
  let costs = Bigarray.(Array2.create int c_layout 2 (len2 + 1)) in
  costs.{0, 0} <- 0;
  for j = 1 to len2 do
    costs.{0, j} <- j * config.ins
  done;
  for i = 1 to len1 do
    let this_i = i land 1
    and prev_i = (i - 1) land 1 in
    costs.{this_i, 0} <- costs.{prev_i, 0} + config.del;
    for j = 1 to len2 do
      let elem1 = arr1.(i - 1)
      and elem2 = arr2.(j - 1) in
      let ul_cost = costs.{prev_i, j - 1} in
      let diag = ul_cost + config.sub elem1 elem2
      and left = costs.{this_i, j - 1} + config.ins
      and top = costs.{prev_i, j} + config.del in
      costs.{this_i, j} <- max diag (max left top)
    done
  done;
  Bigarray.Array2.slice_left costs (len1 land 1)

let write arr =
  for i = 0 to Bigarray.Array1.dim arr - 1 do
    Printf.printf "%4d " arr.{i}
  done;
  Printf.printf "\n"

let partition_y score_l score_r =
  let max_found = ref min_int
  and max_idx = ref (-1) in
  let l_len = Bigarray.Array1.dim score_l
  and r_len = Bigarray.Array1.dim score_r in
  assert (l_len = r_len);
  for i = 0 to l_len - 1 do
    let score = score_l.{i} + score_r.{r_len - 1 - i} in
    if score >= !max_found then begin
      max_found := score;
      max_idx := i
    end
  done;
  assert (!max_idx <> -1);
  !max_idx

let array_rev arr =
  let len = Array.length arr in
  Array.init len (fun i -> arr.(len - 1 - i))

let array_sub_rev arr start len =
  Array.init len (fun i -> arr.(start + len - 1 - i))

let rec hirschberg ?(config=Config.default) arr1 arr2 =
  let len1 = Array.length arr1
  and len2 = Array.length arr2 in
  if len1 = 0 then
    Array.fold_right (fun elem lst -> (None, Some elem) :: lst) arr2 []
  else if len2 = 0 then
    Array.fold_right (fun elem lst -> (Some elem, None) :: lst) arr1 []
  else if len1 = 1 || len2 = 1 then
    needleman_wunsch ~config arr1 arr2
  else begin
    let xmid = len1 / 2 in
    let score_l = nwscore ~config (Array.sub arr1 0 xmid) arr2
    and score_r = nwscore ~config (array_sub_rev arr1 xmid (len1 - xmid))
                          (array_rev arr2) in
    let ymid = partition_y score_l score_r in
    hirschberg (Array.sub arr1 0 xmid) (Array.sub arr2 0 ymid) @
    hirschberg (Array.sub arr1 xmid (len1 - xmid))
               (Array.sub arr2 ymid (len2 - ymid))
  end
