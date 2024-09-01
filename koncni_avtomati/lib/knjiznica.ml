open Avtomat
open Sys
open Array

(* Avtomat sprejema nize oblike 0^n1^n in zavrača vse druge binarne nize. *)
(* let nicle_in_enice = *)
(*   prazen_avtomat "p" 'Z' 'E' *)
(*   |> dodaj_sprejemno_stanje "r" *)
(*   |> dodaj_nesprejemno_stanje "q" *)
(*   |> dodaj_vhodni_simbol '0' *)
(*   |> dodaj_vhodni_simbol '1' *)
(*   |> dodaj_skladovni_simbol 'A' *)
(*   |> dodaj_prehod "p" (Crka '0') (Crka 'Z') "p" ['A'; 'Z'] *)
(*   |> dodaj_prehod "p" (Crka '0') (Crka 'A') "p" ['A'; 'A'] *)
(*   |> dodaj_prehod "p" (Eps ()  ) (Crka 'Z') "q" ['Z'] *)
(*   |> dodaj_prehod "p" (Eps ()  ) (Crka 'A') "q" ['A'] *)
(*   |> dodaj_prehod "q" (Crka '1') (Crka 'A') "q" [] *)
(*   |> dodaj_prehod "q" (Eps ()  ) (Crka 'Z') "r" ['Z'] *)

  (* Mogoče napačen working directory *)
  (* let array_avtomatov = Sys.readdir "../knjiznica-avtomatov" in *)


let rec izberi_datoteko array_avtomatov =
  let n = Array.length array_avtomatov in
  print_endline ("Vnesi število od 0 do " ^ Int.to_string (n - 1));
  for i=0 to (n-1) do
    print_int i;
    print_string ") ";
    print_endline array_avtomatov.(i)
  done;
  print_string "> ";
  let vnos = read_line () in
  try
    let j = int_of_string vnos in
    array_avtomatov.(j)
  with
  | _ -> print_endline ("Napačen vnos, vnesi število od 0 do " ^ Int.to_string (n - 1));
         izberi_datoteko array_avtomatov

let preberi_datoteko str = _
