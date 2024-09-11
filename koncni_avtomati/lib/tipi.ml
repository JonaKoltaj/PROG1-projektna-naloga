open Avtomat

(* Tukaj so shranjeni tipi, ki jih je zaradi preglednosti boljše imeti na enem mestu. *)

type stanje_vmesnika =
  | SeznamMoznosti
  | IzpisAvtomata
  | VnosAvtomata
  | BranjeNiza
  | NalozitevAvtomata

(* Model vsebuje podatke o avtomatu in stanju vmesnika. *)
type model = {
    avtomat : avtomat;
    stanje_vmesnika : stanje_vmesnika;
  }

type msg =
  | PreberiNiz of string
  | ZamenjajVmesnik of stanje_vmesnika
  | ZamenjajAvtomat of avtomat
  | IzpisiAvtomat of avtomat
  | ShraniAvtomat of avtomat
