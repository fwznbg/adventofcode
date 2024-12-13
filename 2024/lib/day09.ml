type file_block = {
  file_id: int;
  total: int;
}

let build_file_block_metadata(disk_map: int list): file_block list = 
  List.mapi (fun (idx: int)(total: int): file_block ->
    match idx mod 2 with
    | 0 -> {file_id=Int.div idx 2; total=total}
    | _ -> {file_id= -1; total=total}
  ) disk_map 

let build_single_file_block ({total;file_id;}: file_block): int list =
  List.init total (fun _ -> file_id)

let single_last_non_free_space_file_block (file_block_metadata: file_block list): (file_block list * file_block) option =
  match file_block_metadata with
  | [] -> None
  | metadata ->
    let reversed = List.rev metadata in
    match reversed with
    | [] -> None
    | first :: second :: tail ->
      if first.file_id = -1 then
        Some (List.rev tail, second)
      else 
        Some (List.rev (second :: tail), first)
    | head :: [] ->
      if head.file_id = -1 then None
      else Some([], head)

let construct_file_blocks_individually(file_block_metadata: file_block list): int list =
  let rec aux (metadata: file_block list) (acc: int list): int list =
    match metadata with
    | [] -> acc
    | head :: tail ->
      let {file_id;total;} = head in
      if file_id = -1 then
        match single_last_non_free_space_file_block tail with
        | None -> acc
        | Some (rest, non_free_space) -> 
          let remaining_space = total - non_free_space.total in
          if remaining_space = 0 then
            aux rest (acc @ (build_single_file_block non_free_space))
          else if remaining_space > 0 then
            aux ( {file_id;total = remaining_space} :: rest) (acc @ (build_single_file_block non_free_space))
          else if remaining_space < 0 then
            aux (rest @ [{file_id=non_free_space.file_id;total=non_free_space.total-total}]) (acc @ (build_single_file_block {file_id=non_free_space.file_id;total}))
          else
        aux tail (acc @ (build_single_file_block head))
      else
        aux tail (acc @ (build_single_file_block head))
  in

  aux file_block_metadata []

let compactible_file_block (file_block_metadata: file_block list) (available_space: int): (file_block option * file_block list) =
  let rec aux (prev: file_block list) (metadata: file_block list) (available_space: int): (file_block option * file_block list) =
    match List.rev metadata with
    | [] -> (None, List.rev prev)
    | h :: tail ->
      if h.total <= available_space then Some (h), (List.rev tail) @ (List.rev prev)
      else aux (prev @ [h]) (List.rev tail) available_space
  in
  
  aux [] file_block_metadata available_space

let construct_file_blocks_whole_files (file_block_metadata: file_block list): int list =
  let rec aux (metadata: file_block list) (to_check_compactibility: file_block list) (acc: int list): int list =
    match metadata with
    | [] -> acc
    | head :: tail ->
      let {file_id;total;} = head in
      if file_id = -1 then
        match compactible_file_block to_check_compactibility total with
        | None, to_check -> 
          aux tail to_check (acc @ (build_single_file_block {file_id=0;total}))
        | Some (compactible), to_check ->
          let remaining_space = total - compactible.total in
          if remaining_space = 0 then
            let compactible_filled_with_free_space = List.map (fun item -> 
              if item.file_id = compactible.file_id then {file_id = -1;total=compactible.total}
              else item
            ) tail in
            aux compactible_filled_with_free_space to_check (acc @ (build_single_file_block compactible))
          else 
            let compactible_filled_with_free_space = List.map (fun item -> 
              if item.file_id = compactible.file_id then {file_id = -1;total=compactible.total}
              else item
            ) tail in
            aux ({file_id = -1;total=remaining_space} :: compactible_filled_with_free_space) to_check (acc @ (build_single_file_block compactible))
      else
        aux tail (List.filter (fun item -> item.file_id <> head.file_id) to_check_compactibility) (acc @ (build_single_file_block head))
  in

  aux file_block_metadata (List.filter (fun item -> item.file_id <> -1) file_block_metadata) []

let checksum(blocks: int list) : int =
  let rec aux(blocks: int list) (idx: int) (acc: int): int =
    match blocks with
    | [] -> acc
    | head :: tail -> 
      aux tail (idx + 1) (acc + (idx * head))
  in
  
  aux blocks 0 0

let run (lines: string list): unit =
  let metadata = List.hd lines 
    |> String.to_seq 
    |> List.of_seq 
    |> List.map (fun item -> int_of_char item - int_of_char '0') 
    |> build_file_block_metadata in
  let individual_compacted = construct_file_blocks_individually metadata in
  let whole_files_compacted = construct_file_blocks_whole_files metadata in
  let part1 = checksum individual_compacted in
  Printf.printf "part 1: %d\n" part1;
  let part2 = checksum whole_files_compacted in
  Printf.printf "part 2: %d\n" part2;
