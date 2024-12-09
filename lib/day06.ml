module CoordSet = Set.Make(struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = 
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | anything -> anything
end)

module CoordDirSet = Set.Make(struct
  type t = (int * int) * (int * int)
  let compare ((x1, y1), (dir_x1, dir_y1)) ((x2, y2), (dir_x2, dir_y2)) = 
    let c = compare x1 x2 in
    if c <> 0 then c else
    let c = compare y1 y2 in
    if c <> 0 then c else
    let c = compare dir_x1 dir_x2 in
    if c <> 0 then c else
    compare dir_y1 dir_y2
end)

type cell = Obstacle | Empty | StartPos

let parse_cell (item: char): cell =
  match item with
  | '#' -> Obstacle
  | '.' -> Empty
  | '^' -> StartPos
  | _ -> raise (Invalid_argument "failed to parse cell")

let parse_lines (lines: string list): cell list list =
  List.map (fun line -> 
    List.of_seq (String.to_seq line) |> List.map (fun r -> parse_cell r)
  ) lines

let find_start_pos (grid: cell list list): int * int =
  let rec loop (grid: cell list list) (row_idx: int): (int * int) option =
    match grid with
    | [] -> None
    | h :: tail ->
      match List.find_index (fun cell -> cell = StartPos) h with
      | Some idx -> Some(row_idx, idx)
      | _ -> loop tail (row_idx + 1) 
  in

  match loop grid 0 with None -> raise (Invalid_argument "invalid input") | Some coord -> coord
  
let is_leaving_grid (grid: cell list list) (coords: int * int): bool =
  let x, y = coords in
  x < 0 || y < 0 || x >= List.length grid || y >= List.length (List.hd grid)

let next_dir (cur_dir: int * int): int * int =
  match cur_dir with
  | -1, 0 -> 0, 1
  | 0, 1 -> 1, 0
  | 1, 0 -> 0, -1
  | 0, -1 -> -1, 0
  | _ -> raise (Invalid_argument "invalid direction")

let coord_set_len (coord_set: CoordSet.t): int = 
  CoordSet.fold (fun _ acc -> acc + 1) coord_set 0

let rec get_route (grid: cell list list) (start_pos: int * int) (direction: int * int) (acc: ((int * int) * (int * int)) list): ((int * int) * (int * int)) list = 
  let x, y = start_pos in
  let cur_x_dir, cur_y_dir = direction in
  let next_x_pos, next_y_pos = (x + cur_x_dir, y + cur_y_dir) in
  if is_leaving_grid grid (next_x_pos, next_y_pos) then acc
  else
    match List.nth (List.nth grid next_x_pos) next_y_pos with
    | Obstacle -> 
      let next_x_dir, next_y_dir = next_dir direction in
      get_route grid start_pos (next_x_dir, next_y_dir) acc
    | _ -> 
      get_route grid (x + cur_x_dir, y + cur_y_dir) direction (((next_x_pos, next_y_pos), direction) :: acc)

let count_route (grid: cell list list) (start_pos: int * int): int = 
  let routes = get_route grid start_pos (-1, 0) ((start_pos, (-1, 0)) :: []) in
  List.fold_left (fun acc (coord, _) -> CoordSet.add coord acc) CoordSet.empty routes
  |> coord_set_len

let rec is_infinite_loop_route (grid: cell list list) (start_pos: int * int) (direction: int * int) (coord_dir_set: CoordDirSet.t): bool = 
  let x, y = start_pos in
  let cur_x_dir, cur_y_dir = direction in
  let next_x_pos, next_y_pos = (x + cur_x_dir, y + cur_y_dir) in
  if is_leaving_grid grid (start_pos) then false
  else if CoordDirSet.mem (start_pos, direction) coord_dir_set then true
  else 
    let updated_set = CoordDirSet.add (start_pos, direction) coord_dir_set in
    if is_leaving_grid grid (next_x_pos, next_y_pos) then false
    else
    match List.nth (List.nth grid next_x_pos) next_y_pos with
    | Obstacle ->
        let next_x_dir, next_y_dir = next_dir direction in
        is_infinite_loop_route grid start_pos (next_x_dir, next_y_dir) updated_set
    | _ ->
      is_infinite_loop_route grid (next_x_pos, next_y_pos) direction updated_set

let set_obstacle_at (x, y: int * int) (grid: cell list list): cell list list =
  let new_grid = List.mapi (fun row_idx row_elem -> 
    if row_idx <> x then row_elem
    else List.mapi (fun col_idx cell_elem -> 
      if col_idx <> y then cell_elem
      else Obstacle
      ) row_elem
    ) grid in
  new_grid

let count_infinite_loop (grid: cell list list) (start_pos: int * int): int =
  let direction = (-1, 0) in
  let route = get_route grid start_pos direction [] in

  let coord_set = CoordSet.of_list (
    List.map (fun (coord, _) -> coord) route
  ) in
  CoordSet.fold (fun coord acc -> 
    let new_grid = set_obstacle_at coord grid in
    if is_infinite_loop_route new_grid start_pos direction CoordDirSet.empty then acc + 1
    else acc
  ) coord_set 0

let run (lines: string list): unit =
  let grid = parse_lines(lines) in
  let start_pos = find_start_pos grid in
  let part1 = count_route grid start_pos in
  let part2 = count_infinite_loop grid start_pos in


  Printf.printf "part 1: %d\n" part1;
  Printf.printf "part 2: %d\n" part2
