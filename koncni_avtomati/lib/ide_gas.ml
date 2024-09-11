open Avtomat
(* Če to ne dela napiši Avtomat.stanje ipd *)

(* Nemudni opis opiše trenutno stanje avtomata*)
type nemudni_opis =
  {
    stanje : stanje;
    vhodni_niz : char list;
    sklad : char list;
  }

(* Funkcija sprejme nemudni_opis ter boolean, ki nam pove, ali naj avtomat prebere naslednji znak vhodnega niza.
   Funkcija vrne None, če koraka ni moč izvesti, in Some nemudni_opis, če avtomat ta korak lahko izvede. *)
let en_korak avtomat nemudni_opis preberi =
  (* Preberemo vrhni skladovni simbol. Če je sklad prazen, preberemo prazni niz.*)
  let skladovni_simbol =
    match nemudni_opis.sklad with
    | [] -> Eps ()
    | g :: _ -> Crka g
  in
  (* Pomožna funkcija, ki pri danem vhodnem simbolu in izhodnem nizu izračuna novi nemudni_opis.*)
  let aux vhodni_simbol izhodni_niz =
    match prehodna_funkcija avtomat nemudni_opis.stanje vhodni_simbol skladovni_simbol with
    | None -> None
    | Some (stanje2, skladovni_niz) ->
       let preostanek_skladu =
         match nemudni_opis.sklad with
         | [] -> []
         | _ :: y -> y
       in
       Some {
           stanje = stanje2;
           vhodni_niz = izhodni_niz;
           sklad = skladovni_niz @ preostanek_skladu
         }
  in
  match nemudni_opis.vhodni_niz with
  (* Če je vhodni niz nemudnega opisa prazen in želimo prebrati simbol, vrnemo None, v nasprotnem primeru pomožno funkcijo poženemo s praznim nizom.*)
  | [] -> if preberi then None
          else aux (Eps ()) []
  (* Glede na to, ali želimo simbol prebrati, pomožno funkcijo poženemo z ustreznim vhodnim simbolom in izhodnim nizom. *)
  | a :: x ->
     let vhodni_simbol = if preberi then Crka a else Eps () in
     let izhodni_niz = if preberi then x else a :: x in
     aux vhodni_simbol izhodni_niz


(* Prehodni funkciji med nizi in seznami simbolov. *)
let explode s = List.init (String.length s) (String.get s)
let implode l = String.of_seq (List.to_seq l)

(* Funkciji, ki za boljšo preglednost izhoda lepo izpiše nemudni opis. *)
let pretty_print_nemudni_opis avtomat nemudni_opis =
  let print_vhodni_niz =
    match nemudni_opis.vhodni_niz with
    | [] -> String.make 1 avtomat.prazni_simbol
    | _ -> implode nemudni_opis.vhodni_niz
  in
  let print_sklad =
    match nemudni_opis.sklad with
    | [] -> String.make 1 avtomat.prazni_simbol
    | _ -> implode nemudni_opis.sklad
  in
  print_string ("(" ^ nemudni_opis.stanje ^ ", " ^ print_vhodni_niz ^ ", " ^ print_sklad ^ ")")

let print_nemudni_opisi_list avtomat l =
  let rec aux = function
    | [] -> print_endline ""
    | [x] -> pretty_print_nemudni_opis avtomat x
    | x :: xs -> pretty_print_nemudni_opis avtomat x; print_string " -> "; aux xs
  in
  aux (List.rev l)

(* Na nizu poženemo dani avtomat. Funkcija vrne true, če avtomat niz sprejme in false sicer.
   Ker na vsakem koraku simbol iz vhodnega niza lahko preberemo ali ne, avtomat ni determinističen. Funkcija se zato drevesno spusti v vse možnosti.
   Na koncu vsake veje, ko dobimo resničnostno vrednost, funkcija tudi izpiše zaporedje nemudnih opisov, ki je vodilo do vrednosti. *)
let pozeni avtomat niz =
  let vhodni_niz = explode niz in
  (* Iz danega avtomata preberemo zacetni nemudni opis. *)
  let zacetni_nemudni_opis =
    {
      stanje = avtomat.zacetno_stanje;
      vhodni_niz;
      sklad = [ avtomat.zacetni_skladovni_simbol ];
    }
  in
  let rec aux acc nemudni_opis =
    match nemudni_opis.vhodni_niz with
    (* V primeru ko je vhodni niz prazen preverimo, ali je avtomat v zaključnem stanju. *)
    | [] -> (
      match nemudni_opis.sklad with
      (* Če je sklad prazen, zaključimo in sprejmemo niz. *)
      | [] -> print_nemudni_opisi_list avtomat acc; print_endline " ★"; true
      (* Če sklad ni prazen, preverimo, ali smo v enem od sprejemnih stanj. *)
      | _  -> if List.mem nemudni_opis.stanje avtomat.sprejemna_stanja then
                (print_nemudni_opisi_list avtomat acc; print_endline " ★"; true)
              else
                (* Če v sprejemnem stanju še nismo, lahko avtomat poženemo na praznem nizu. *)
                match en_korak avtomat nemudni_opis false with
                | None -> print_nemudni_opisi_list avtomat acc; print_endline ""; false
                | Some nemudni_opis' -> aux (nemudni_opis' :: acc) nemudni_opis'
    )
    | _ ->
       (* Glede na to, ali preberemo simbol, se spustimo v obe možnosti. *)
       let en_korak_preberi =
         match en_korak avtomat nemudni_opis true with
         | None -> print_nemudni_opisi_list avtomat acc; print_endline ""; false
         | Some nemudni_opis' -> aux (nemudni_opis' :: acc) nemudni_opis'
       in
       let en_korak_ne_preberi =
         match en_korak avtomat nemudni_opis false with
         | None -> print_nemudni_opisi_list avtomat acc; print_endline ""; false
         | Some nemudni_opis' -> aux (nemudni_opis' :: acc) nemudni_opis'
       in
       (* Da bi avtom niz sprejel, mora uspeti vsaj ena veja. *)
       en_korak_preberi || en_korak_ne_preberi
  in
  aux [zacetni_nemudni_opis] zacetni_nemudni_opis
