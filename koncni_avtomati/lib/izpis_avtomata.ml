open Avtomat
open Ide_gas

let print_list lst =
  print_string "{";
  let rec aux = function
    | [] -> print_endline "}"
    | [x] -> print_string x; print_endline "}"
    | x :: xs -> print_string (x ^ ", "); aux xs
  in
  aux lst

let crka_ali_eps_to_str avtomat = function
  | Eps () -> String.make 1 (avtomat.prazni_simbol)
  | Crka x -> String.make 1 x

let skladovni_niz_v_ta_pravi_niz avtomat = function
  | [] -> String.make 1 avtomat.prazni_simbol
  | x -> implode x

let rec print_prehodi avtomat = function
  | [] -> ()
  | (s1, vsimb, ssimb, s2, chars) :: xs ->
     print_string ("(" ^ s1 ^ ", ");
     print_string ((crka_ali_eps_to_str avtomat vsimb) ^ ", ");
     print_string ((crka_ali_eps_to_str avtomat ssimb) ^ ", ");
     print_string " -> ";
     print_string ("(" ^ s2 ^ ", ");
     print_endline ((skladovni_niz_v_ta_pravi_niz avtomat chars) ^ ")");
     print_prehodi avtomat xs

let izpisi_avtomat avtomat =
  print_string "• Stanja: Q = "; print_list avtomat.stanja;
  print_string "• Vhodna abeceda: Σ = "; print_list (List.map (String.make 1) avtomat.vhodna_abeceda);
  print_string "• Skladovna abeceda: Γ = "; print_list (List.map (String.make 1) avtomat.skladovna_abeceda);
  print_string "• Začetno stanje: "; print_endline avtomat.zacetno_stanje;
  print_string "• Začetni skladovni simbol: "; print_char avtomat.zacetni_skladovni_simbol; print_endline "";
  print_string "• Prazni simbol: "; print_char avtomat.prazni_simbol; print_endline "";
  print_string "• Sprejemna stanja: F = "; print_list avtomat.sprejemna_stanja;
  print_endline "• Prehodi: δ = {"; print_prehodi avtomat avtomat.prehodna_relacija; print_endline "  }"
