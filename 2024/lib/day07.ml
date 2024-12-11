type equation = {
  total: int;
  nums: int list
}
type operation = int -> int -> int

let parse_input(input: string): int * int list =
  let k = Scanf.sscanf input "%d:" (fun k -> k) in
  let v = (List.nth (String.split_on_char ':' input ) 1)|> String.trim in
  k, List.map (fun item -> int_of_string item) (String.split_on_char ' ' v)

let build_input_map(inputs: string list): equation list = 
  let rec aux(inputs: string list) (acc: equation list) =
    match inputs with
    | [] -> acc
    | h :: t -> 
      let k, v = parse_input h in
      aux t ({total = k; nums = v} :: acc)
  in 

  aux inputs []

let is_equation_possibly_correct (total: int) (nums: int list) (oprs: operation list): bool =
  let rec aux (target: int) (equations: int list) (acc: int): bool =
    match equations with
    | [] ->
      acc = target
    | h :: tail ->
      if acc > target then false
      else
        List.exists (fun opr -> aux target tail (opr acc h)) oprs
  in

  aux total (List.tl nums) (List.hd nums)

let count_possible_correct_equation (eq: equation list) (oprs: operation list): int =
  List.fold_left (fun acc {total; nums; } -> 
    if is_equation_possibly_correct total nums oprs then acc + total
    else acc
    ) 0 eq

let run (inputs: string list): unit =
  let input_map = build_input_map inputs in
  let count_possible_correct_equation_with = count_possible_correct_equation input_map in 
  let concat_int (a: int) (b: int): int =
    int_of_string ((string_of_int a) ^ (string_of_int b))
  in 
  let part1 = count_possible_correct_equation_with [Int.add; Int.mul] in
  let part2 = count_possible_correct_equation_with [Int.add; Int.mul; concat_int]  in


  Printf.printf "part 1: %d\n" part1;
  Printf.printf "part 2: %d\n" part2;
