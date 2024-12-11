let build_reports_aux (line: string): int list =
  let nums = String.split_on_char ' ' line in
  List.map int_of_string nums

let rec build_reports (lines: string list) (acc: int list list): int list list = 
  match lines with
  | [] -> acc
  | h :: t -> build_reports t (build_reports_aux h :: acc)

let is_report_safe (report: int list) : bool = 
  let rec increasing (report: int list) (acc: bool): bool = 
    match report with
    | [] | [_] -> acc
    | h1 :: (h2 :: _ as tail) ->
      h2 - h1 >= 1 && h2 - h1 <= 3 && (increasing tail acc)
  and decreasing (report: int list) (acc: bool): bool = 
    match report with
    | [] | [_] -> acc
    | h1 :: (h2 :: _ as tail) ->
      h1 - h2 >= 1 && h1 - h2 <= 3 && (decreasing tail acc) in
 
  match report with
    | [] | [_] -> true
    | h1 :: h2 :: _ ->
      if h1 >= h2 then decreasing report true 
      else increasing report true

let is_report_safe_by_removing (lines: int list): bool =
  let remove_nth_elem (lines: int list) (nth: int): int list =
    let rec aux (lines: int list) (i: int) (acc: int list): int list =
      match lines with
      | [] -> acc
      | h :: t -> 
        if nth = i then aux t (i+1) acc
        else aux t (i+1) (h :: acc)
      in

    List.rev (aux lines 0 [])
    in
  
  let len = List.length lines in
  let rec is_safe (lines: int list) (idx: int): bool =
    if is_report_safe (remove_nth_elem lines idx) then true
    else if idx = len then false
    else is_safe lines (idx+1)
  in

  is_safe lines 0

let run (lines: string list): unit = 
  let reports = build_reports lines [] in
  let part1 = List.fold_left (fun acc l -> if is_report_safe l then acc + 1 else acc) 0 reports in
  let part2 = List.fold_left (fun acc l -> if is_report_safe_by_removing l then acc + 1 else acc) 0 reports in

  Printf.printf "part 1: %d \n" part1;
  Printf.printf "part 2: %d \n" part2
