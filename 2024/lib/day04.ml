let count_xmas (lines: string list): int =
  let target = "XMAS" in
  let rec is_xmas (row: int) (col: int) (r_offset: int) (c_offset: int) (target_offset: int): bool =
    if target_offset = 4 then true
    else if row < 0 || col < 0 || row >= (List.length lines) || col >= (String.length (List.hd lines)) then false
    else if not (Char.equal (List.nth lines row).[col] target.[target_offset]) then false
    else is_xmas (row + r_offset) (col + c_offset) r_offset c_offset (target_offset + 1)
  in

  let count_from (row: int) (col: int): int =
    let offset = [-1,-1;-1,0;-1,1;0,-1;0,1;1,-1;1,0;1,1] in
    List.fold_left (fun acc (r_offset, c_offset) ->
      if is_xmas row col r_offset c_offset 0 then acc + 1 else acc) 0 offset
  in

  let rec loop (row: int) (col: int) (acc: int): int =
    if row >= List.length lines then acc
    else if col >= String.length (List.hd lines) then loop (row + 1) 0 acc
    else loop row (col + 1) (acc + count_from row col)
  in

  loop 0 0 0

let count_cross_mas (lines: string list): int =
  let rec check_char (row: int) (col: int) (target_char: char): bool =
    if row < 0 || col < 0 || row >= (List.length lines) || col >= (String.length (List.hd lines)) then false
    else
      let char_to_check = (List.nth lines row).[col] in
      if char_to_check <> target_char then false
      else
      match char_to_check with
      | 'A' -> 
        let top_left = check_char (row - 1) (col - 1) in
        let top_right = check_char (row - 1) (col + 1) in
        let bottom_left = check_char (row + 1) (col - 1) in
        let bottom_rigth = check_char (row + 1) (col + 1) in

        let top_left_to_bottom_rigth = 
          (top_left 'M' && bottom_rigth 'S') ||
          (top_left 'S' && bottom_rigth 'M') 
        in

        let bottom_left_to_top_right =
          (bottom_left 'M' && top_right 'S') || 
          (bottom_left 'S' && top_right 'M')
        in

        top_left_to_bottom_rigth && bottom_left_to_top_right
      | 'S' | 'M' -> true
      | _ -> false
  in

  let rec loop (row: int) (col: int) (acc: int): int =
    if row >= List.length lines then acc
    else if col >= String.length (List.hd lines) then loop (row + 1) 0 acc
    else loop row (col + 1) (if check_char row col 'A' then acc + 1 else acc)
  in

  loop 0 0 0


let run (lines: string list): unit =
  let part1 = count_xmas lines in
  Printf.printf "part 1: %d\n" part1;
  let part2 = count_cross_mas lines in

  Printf.printf "part 2: %d\n" part2
