module IntMap = Map.Make(Int)

let opt_to_val (opt: 'a option): 'a =
  match opt with
  | None -> raise (Invalid_argument "invalid input for opt_to_val") 
  | Some v -> v

let parse_rules (line: string): int * int = Scanf.sscanf line "%d|%d" (fun x y -> x, y)


let build_input (lines: string list): int list IntMap.t * int list list = 
  let empty_idx = List.find_index (fun line -> String.length line = 0) lines |> opt_to_val in
  let raw_rules = List.filteri (fun cur_idx _ -> cur_idx < empty_idx) lines in
  let raw_input = List.filteri (fun cur_idx _ -> cur_idx > empty_idx) lines 
    |> List.map (fun input -> String.split_on_char ',' input) in


  let add_rules (rules: int list IntMap.t) (rule: string): int list IntMap.t = 
    let num1, num2 = parse_rules rule in
    let updated_rules = IntMap.update num1 (fun x ->
      match x with
      | None -> Some [num2]
      | Some y -> Some (num2 :: y)
      ) rules in

      updated_rules
    in
    
    let rules = List.fold_left add_rules IntMap.empty raw_rules in
    let inputs = List.map (fun raw_input -> 
      List.map int_of_string raw_input
    ) raw_input in 


    rules, inputs

let sort_update (update: int list) (rules: int list IntMap.t) = 
  let compare a b =
    match IntMap.find_opt a rules with
    | Some l when List.mem b l -> -1
    | _ ->
      match IntMap.find_opt b rules with
      | Some l when List.mem a l -> 1
      | _ -> 0

  in
  List.sort compare update

let get_median (update: int list): int = 
  List.nth update (List.length update / 2)

let sum_sorted (updates: int list list) (rules: int list IntMap.t): int =
  let rec loop (updates: int list list) (acc: int) =
    match updates with
    | [] -> acc
    | h :: t ->
      if sort_update h rules = h then loop t (acc + get_median h)
      else loop t acc
    in

  loop updates 0

  let sum_unsorted (updates: int list list) (rules: int list IntMap.t): int =
    let rec loop (updates: int list list) (acc: int) =
      match updates with
      | [] -> acc
      | h :: t ->
        let sorted = sort_update h rules in
        if sorted <> h then loop t (acc + get_median sorted)
        else loop t acc
      in
  
    loop updates 0

let run (lines: string list): unit =
  let rules, input = build_input lines in
  let part1 = sum_sorted input rules in
  let part2 = sum_unsorted input rules in


  Printf.printf "part 1: %d\n" part1;
  Printf.printf "part 2: %d\n" part2
