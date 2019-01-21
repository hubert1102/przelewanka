(* Autor rozwiązania: Hubert Budzyński *)
(* Recenzent: Hugo Dutka *)

open Hashtbl;;
open Array;;
open Queue;;


let rec nwd a b = if a = 0 then b else nwd (b mod a) a;;


let przelewanka t =
  let hash = Hashtbl.create 1696969 
  and n = Array.length t in
  let tab = init n (fun i -> fst t.(i)) 
  and kon = init n (fun i -> snd t.(i))
  and start = make n 0 in
  let x = fold_left nwd tab.(0) tab
  in
  (* sprawdzam dwa warunki konieczne, aby udało się uzyskać stan końcowy: 
  NWD wszystkich pojemności musi dzielić każdy ze stanow końcowych i
  ktoryś stan końcowy musi wynosić 0 albo całkowita pojemność danej szklanki *)
  if not (for_all (fun h -> h mod x = 0) kon) || (not (exists ((=) 0) kon) && not (exists (fun (a, b) -> a = b) t)) then -1 else
  let dolej tb x =
  (
    tb.(x) <- tab.(x);
    tb
  )
  and wylej tb x =
  (
    tb.(x) <- 0;
    tb
  )
  and przelej tb x y =
  (
    if tb.(y) + tb.(x) <= tab.(y) then 
    (
      tb.(y) <- tb.(y) + tb.(x);
      tb.(x) <- 0;
    )
    else
    (
      tb.(x) <- tb.(x) - tab.(y) + tb.(y);
      tb.(y) <- tab.(y);
    );
    tb
  ) 
  in
  (* w kolejce q trzymam wszystkie uzyskane i nieprzetworzone stany szklanek,
  jeżeli uda się dostać nieuzyskany wcześniej stan, to dodaję go do kolejki i 
  zapamiętuję po ilu krokach go uzyskałem w hashtable, dla każdego uzyskanego 
  stanu znajduję wszystkie stany, jakie mogę z niego osiągnąć jedną operacją *)
  let q = create () and znalezione = ref false in
  (
    add start q;
    if start = kon then znalezione := true;
    Hashtbl.add hash start 0;
    while (not (is_empty q) && not !znalezione) 
    do
      let t = take q in 
      (
        for i = 0 to n-1 do
          if not (Hashtbl.mem hash (dolej (Array.copy t) i)) then (
            if dolej (Array.copy t) i = kon then znalezione := true;
            Hashtbl.add hash (dolej (Array.copy t) i) ((find hash t) + 1);
            add (dolej (Array.copy t) i) q;
          );
          if not (Hashtbl.mem hash (wylej (Array.copy t) i)) then (
            if wylej (Array.copy t) i = kon then znalezione := true;
            Hashtbl.add hash (wylej (Array.copy t) i) ((find hash t) + 1);
            add (wylej (Array.copy t) i) q;
          );
          if t.(i) <> 0 then for j = 0 to n-1 do
            if not (Hashtbl.mem hash (przelej (Array.copy t) i j)) then (
              if przelej (Array.copy t) i j = kon then znalezione := true;
              Hashtbl.add hash (przelej (Array.copy t) i j) ((find hash t) + 1);
              add (przelej (Array.copy t) i j) q;
            );
          done;
        done;
      );
    done;
    if !znalezione then find hash kon
    else -1
  )
;;
