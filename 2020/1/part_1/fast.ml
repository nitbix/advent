open Core;;

let filename = "input.txt" in
let flines = In_channel.read_lines filename in
let entries = List.map flines ~f:Int.of_string in

let seen = Hash_set.create ~size:(List.length entries) (module Int) in
let () = List.iter entries ~f:(fun elem -> Hash_set.add seen elem) in

let rec check a =
  match a with
  | [] -> None
  | hd_a :: tl_a -> 
      if (Hash_set.mem seen (2020 - hd_a)) then Some (hd_a * (2020 - hd_a)) else check tl_a
in

let r = match (check entries) with
| Some x -> Int.to_string x
| None -> "not found"
in 

print_string r
