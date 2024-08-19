(*Koncni avtomati s skladom*)

type stanje = string
type crka_ali_eps = Crka of char | Eps of unit

type avtomat = {
    stanja : stanje list;
    vhodna_abeceda : char list;
    skladovna_abeceda : char list;
    (* Prehodno relacijo podamo kot relacijo med stanjem, vhodnim simbolom ter skladovnim simbolom in izhodnim stanjem ter izhodnim skladovnim nizom.
       Vhodni simbol in skladovni simbol sta lahko tudi prazna.*)
    prehodna_relacija :
      (stanje * crka_ali_eps * crka_ali_eps * stanje * (char list)) list;
    zacetno_stanje : stanje;
    zacetni_skladovni_simbol : char;
    sprejemna_stanja : stanje list;
  }

let prazen_avtomat zacetno_stanje zacetni_skladovni_simbol =
  {
    stanja = [ zacetno_stanje ];
    vhodna_abeceda = [];
    skladovna_abeceda = [ zacetni_skladovni_simbol ];
    prehodna_relacija = [];
    zacetno_stanje;
    zacetni_skladovni_simbol;
    sprejemna_stanja = [];
  }

let dodaj_nesprejemno_stanje stanje avtomat =
  { avtomat with stanja = stanje :: avtomat.stanja }

let dodaj_vhodni_simbol simbol avtomat =
  { avtomat with vhodna_abeceda = simbol :: avtomat.vhodna_abeceda }

let dodaj_skladovni_simbol simbol avtomat =
  { avtomat with skladovna_abeceda = simbol :: avtomat.skladovna_abeceda }

let dodaj_prehod stanje1 vhodni_simbol skladovni_simbol stanje2 skladovni_niz avtomat =
  { avtomat with prehodna_relacija = (stanje1, vhodni_simbol, skladovni_simbol, stanje2, skladovni_niz) :: avtomat.prehodna_relacija }

let dodaj_sprejemno_stanje stanje avtomat =
  {
    avtomat with
    stanja = stanje :: avtomat.stanja;
    sprejemna_stanja = stanje :: avtomat.sprejemna_stanja;
  }

(* Funkcija preveri, ali je prehod z danim stanjem, vhodnim simbol ter skladovnim simbolom mogooč in vrne izhodno stanje ter izhodni skladovni niz*)
let prehodna_funkcija avtomat stanje vhodni_simbol skladovni_simbol =
  match
    List.find_opt
      (fun (stanje1, vhodni_simbol', skladovni_simbol', _, _) -> stanje1 = stanje && vhodni_simbol = vhodni_simbol' && skladovni_simbol = skladovni_simbol')
      avtomat.prehodna_relacija
  with
  | None -> None
  | Some (_, _, _, stanje2, skladovni_niz) -> Some (stanje2, skladovni_niz)
