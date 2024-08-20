open Koncni_avtomati.Knjiznica
open Koncni_avtomati.Tekstovni_vmesnik


let _ =
  print_endline "Program je naložen z avtomatom, ki prepoznava nize oblike 0^n1^n";
  loop { avtomat = nicle_in_enice; stanje_vmesnika = SeznamMoznosti }

(* let test2 = *)
(*   let rg = Str.regexp "(\\(.+?\\), \\(.\\), \\(.\\))" in *)
(*   let _ = Str.string_match rg "(p, 0, Z)" 0 in *)
(*   print_endline (Str.matched_group 1 "(p, 0, Z)") *)
