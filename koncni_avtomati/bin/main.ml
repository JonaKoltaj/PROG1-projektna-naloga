open Koncni_avtomati.Avtomat
open Koncni_avtomati.Ide_gas

(* Avtomat sprejema nize oblike 0^n1^n in zavrača vse druge binarne nize. *)
let nicle_in_enice =
  prazen_avtomat "p" 'Z'
  |> dodaj_sprejemno_stanje "r"
  |> dodaj_nesprejemno_stanje "q"
  |> dodaj_vhodni_simbol '0'
  |> dodaj_vhodni_simbol '1'
  |> dodaj_skladovni_simbol 'A'
  |> dodaj_prehod "p" (Crka '0') (Crka 'Z') "p" ['A'; 'Z']
  |> dodaj_prehod "p" (Crka '0') (Crka 'A') "p" ['A'; 'A']
  |> dodaj_prehod "p" (Eps ()  ) (Crka 'Z') "q" ['Z']
  |> dodaj_prehod "p" (Eps ()  ) (Crka 'A') "q" ['A']
  |> dodaj_prehod "q" (Crka '1') (Crka 'A') "q" []
  |> dodaj_prehod "q" (Eps ()  ) (Crka 'Z') "r" ['Z']
(* let test = pozeni nicle_in_enice "00001111" *)

(* let () = print_endline (Bool.to_string test) *)






let testni_model =
  {
    avtomat = nicle_in_enice;
    stanje_avtomata = nicle_in_enice.zacetno_stanje;
    sklad = [ nicle_in_enice.zacetni_skladovni_simbol ];
    stanje_vmesnika = VnosAvtomata;
    prazni_simbol = 'E';
  }

let test = vnesi_prehod testni_model
(* let test2 = *)
(*   let rg = Str.regexp "(\\(.+?\\), \\(.\\), \\(.\\))" in *)
(*   let _ = Str.string_match rg "(p, 0, Z)" 0 in *)
(*   print_endline (Str.matched_group 1 "(p, 0, Z)") *)
