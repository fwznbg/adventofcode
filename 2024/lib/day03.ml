let mul_it_over (line: string): int =
  let re = Str.regexp {|mul(\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\))|} in
  let rec get_all_num (line: string) (idx: int) (acc: string list): string list =
    match Str.search_forward re line idx with
    | exception Not_found -> acc 
    | _ -> 
      get_all_num line (Str.match_end ()) (Str.matched_group 1 line :: acc)
  in

  let res = get_all_num line 0 [] 
    |> List.map (fun item -> Scanf.sscanf item "%d,%d" (fun x y -> (x * y))) 
    |> List.fold_left (+) 0
  in

  res

let uncorrupted_mul_it_over (line: string) (enabled: bool): int * bool =
  let re = Str.regexp {|mul([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?)\|don't()\|do()|} in

  let rec get_all_num (line: string) (idx: int) (enabled: bool) (acc: string list): string list * bool =
    match Str.search_forward re line idx with
    | exception Not_found -> acc, enabled
    | _ -> 
      let matched_substring = Str.matched_string line in
      match matched_substring with
      | "do()" -> get_all_num line (Str.match_end ()) true acc
      | "don't()" -> get_all_num line (Str.match_end ()) false acc
      | _ ->
        if enabled then get_all_num line (Str.match_end ()) enabled (matched_substring :: acc)
        else get_all_num line (Str.match_end ()) enabled acc
          
  in

  let all_num, enabled = get_all_num line 0 enabled [] in
  let res = all_num
    |> List.map (fun item -> Scanf.sscanf item "mul(%d,%d)" (fun x y -> (x * y))) 
    |> List.fold_left (+) 0
  in

  res, enabled


let run (lines: string list): unit =
  let part1 = 
    let rec aux (l: string list) (acc: int) : int = 
      match l with
      | h :: t -> aux t ((mul_it_over h) + acc) 
      | [] -> acc
    in
   
    aux lines 0 in
  
  let part2 = 
    let rec aux (l: string list) (enabled: bool) (acc: int) : int = 
      match l with
      | h :: t ->
        let res, enabled = uncorrupted_mul_it_over h enabled in
        aux t enabled (res + acc) 
      | [] -> acc
    in
   
    aux lines true 0 in

  Printf.printf "part 1: %d\n" part1;
  Printf.printf "part 2: %d\n" part2
