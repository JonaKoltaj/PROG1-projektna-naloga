open Koncni_avtomati.Avtomat
open Koncni_avtomati.Ide_gas

let nicle_in_enice =
  prazen_avtomat "p" (Crka 'Z')
  |> dodaj_sprejemno_stanje "r"
  |> dodaj_nesprejemno_stanje "q"
  |> dodaj_vhodni_simbol (Crka '0')
  |> dodaj_vhodni_simbol (Crka '1')
  |> dodaj_skladovni_simbol (Crka 'A')
