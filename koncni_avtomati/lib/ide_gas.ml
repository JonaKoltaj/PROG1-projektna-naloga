open Avtomat
(* Če to ne dela napiši Avtomat.stanje ipd *)

type nemudni_opis =
  {
    stanje : stanje;
    vhodni_niz : char list;
    sklad : char list;
  }

let en_korak avtomat nemudni_opis preberi =
  match nemudni_opis.vhodni_niz with
  | [] -> Some nemudni_opis
  | a :: x ->
     let skladovni_simbol =
       match nemudni_opis.sklad with
       | [] -> Prazni_niz ()
       | g :: _ -> Crka g
     in
     let vhodni_simbol : vhodna_abeceda = if preberi then Crka a else Prazni_niz () in
     match prehodna_funkcija avtomat nemudni_opis.stanje vhodni_simbol skladovni_simbol with
     | None -> None
     | Some (stanje2, skladovni_niz) ->
        let preostanek_skladu =
          match nemudni_opis.sklad with
          | [] -> []
          | _ :: y -> y
        in
        Some {
          nemudni_opis with
          stanje = stanje2;
          vhodni_niz = x;
          sklad = skladovni_niz @ preostanek_skladu
          }

let explode s = List.init (String.length s) (String.get s)

let pozeni avtomat niz =
  let vhodni_niz = explode niz in
  let zacetni_nemudni_opis =
    {
      stanje = avtomat.zacetno_stanje;
      vhodni_niz;
      sklad = [ avtomat.zacetni_skladovni_simbol ];
    }
  in
  let rec aux nemudni_opis =
    match nemudni_opis.vhodni_niz with
    | [] -> (
      match nemudni_opis.sklad with
      | [] -> True
      | _  -> List.mem nemudni_opis.stanje avtomat.sprejemna_stanja
    )
    | _ ->
       let en_korak_preberi =
         match en_korak avtomat nemudni_opis True with
         | None -> False
         | Some nemudni_opis' -> aux nemudni_opis'
       in
       let en_korak_ne_preberi =
         match en_korak avtomat nemudni_opis False with
         | None -> False
         | Some nemudni_opis' -> aux nemudni_opis'
       in
       en_korak_preberi || en_korak_ne_preberi
  in
  aux zacetni_nemudni_opis
