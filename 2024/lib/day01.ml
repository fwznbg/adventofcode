let extract_value (line: string) : int * int = Scanf.sscanf line "%d   %d" (fun x y -> (x, y))

let rec build_lines (lines: string list) (acc1: int list) (acc2: int list): (int list * int list) =
  match lines with
  | [] -> (acc1, acc2)
  | line :: rest -> 
    let (num1, num2) = extract_value line in
    build_lines rest (num1 :: acc1) (num2 :: acc2)

let rec distance (num1: int list) (num2: int list) (acc: int): int =
  match (num1, num2) with
    | ([], []) -> acc
    | h1::t1, h2::t2 -> distance t1 t2 (acc + if h1 > h2 then h1 - h2 else h2 - h1)
    | _, _ -> acc

let rec n_similar (num2: int list) (item: int) (acc: int): int =
  match num2 with
  | [] -> acc
  | head :: tail when head = item -> n_similar tail item acc + 1
  | _ :: tail -> n_similar tail item acc

let rec sum_similarity (num1: int list) (num2: int list) (acc: int): int =
  match num1 with
  | [] -> acc
  | head :: tail -> 
    let n = n_similar num2 head 0 in
    sum_similarity tail num2 (acc + (n * head))

let run (lines: string list): unit = 
  let (num1, num2) = build_lines lines [] [] in
  let sorted_num1 = List.sort compare num1 in
  let sorted_num2 = List.sort compare num2 in

  let part1 = distance sorted_num1 sorted_num2 0 in
  let part2 = sum_similarity num1 num2 0 in

  Printf.printf "part 1: %d\n" part1;
  Printf.printf "part 2: %d\n" part2
