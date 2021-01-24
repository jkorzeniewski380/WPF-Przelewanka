(* ======= Autor: Jakub Korzeniewski ======= *)
(* ============= Przelewanka =============== *)
(* ====== Code Review: Barbara Rosiak ====== *)

(* == Testy: https://gitlab.com/MIMUW-wpf/testy-przelewanka/-/tree/master/tests == *)


(** funkcja pomocnicza wyznaczająca największy wspólny dzielnik [x] i [y] 
    int -> int -> int *)
let rec gcd x y =
  if x = 0 then y else gcd (y mod x) x;;


(** funkcja sprawdzająca czy możliwe jest uzyskanie sytuacji opisanej na wejściu 
    Aby sytuacja była osiągalna, muszą być spełnione dwa warunki:
    - Co najmniej jedna szklanka na wyjściu musi być pusta lub pełna
    - W końcowym stanie ilość wody w każdej szklance musi być podzielna przez 
        największy wspólny dzielnik wszystkich pojemności szklanek
      (int * int) array -> bool *)
let canBeDone input = 
  let emptyOrFull = Array.exists (fun (x, y) -> x = y || y = 0) input in

  if emptyOrFull then
    let inputGCD = Array.fold_left (fun a (x, _) -> gcd a x) 0 input in
    let doesntDivideAll = Array.exists (fun (_, y) -> y mod inputGCD <> 0) input in
    not doesntDivideAll
  else false;;

(** funkcja szukająca wyniku na podstawie par [stan, liczba_krokow] w kolejce [que]
    (int array, bool) Hashtbl.t ->
    (int array * int) Queue.t ref -> int array -> int array -> int -> int *)
let backtrack states que x y n =
  let (state, steps) = Queue.pop !que in

  (* jeśli aktualny stan był już rozważany, nie sprawdzamy go ponownie *)
  if Hashtbl.mem states state then -1
  else
    (* jeśli aktualny stan jest taki sam jak oczekiwany, zwracamy wynik *)
  if state = y then steps
  else
    begin
      (* oznaczamy aktualny stan jako sprawdzony *)
      Hashtbl.add states state true;

      (* dodajemy wszystkie stany powstałe przez napełnienie jednej z pustych
          szklanek do kolejki *)
      for i = 0 to (n - 1) do
        if state.(i) = 0 then 
          begin
            let newState = Array.copy state in
            newState.(i) <- x.(i);
            Queue.push (newState, steps + 1) !que
          end;
      done;

      (* dodajemy wszystkie stany powstałe przez opróżnienie jednej z pełnych
          szklanek do kolejki *)
      for i = 0 to (n - 1) do
        if state.(i) = x.(i) then
          begin
            let newState = Array.copy state in
            newState.(i) <- 0;
            Queue.push (newState, steps + 1) !que
          end;
      done;

      (* dodajemy wszystkie stany powstałe przez opróżnienie jednej ze szklanek 
          napełnionej do pewnego stopnia (ale nie do pełna) do innej szklanki (która nie jest zapełniona)
          do kolejki *)
      for i = 0 to (n - 1) do
        for j = 0 to (n - 1) do
          if i <> j && state.(i) <> 0 && state.(j) <> x.(j) then
            begin
              let newState = Array.copy state
              and change = min (x.(j) - state.(j)) (state.(i)) in
              newState.(i) <- newState.(i) - change;
              newState.(j) <- newState.(j) + change;
              Queue.push (newState, steps + 1) !que
            end
        done;
      done;
      -1
    end;;

(** funkcja wyznaczająca dla tablicy par (x, y) długości n, w ilu krokach 
    można napełnić puste szklanki o pojemnościach x1, x2, ..., xn w taki sposób, 
    że w i-tej szklance znajduje się yi wody. Jeśli jest to nieosiągalne, funkcja zwraca -1
    (int * int) array -> int *)
let przelewanka input = 
  let n = Array.length input in 

  (* corner case - puste wejście *)
  if n = 0 then 0
  else
    (* rozdzielamy tablicę z wejścia na dwie tablice:
        x - tablica pojemności szklanek 
        y - tablica oczekiwanych ilości wody w szklankach *)
    let x = Array.init n (fun i -> fst input.(i))
    and y = Array.init n (fun i -> snd input.(i)) in

    (* corner case - wszystkie szklanki mają być pełne:
        zliczamy wszystkie szklanki o pojemności większej niż 0 *)
    if x = y then 
      Array.fold_left (fun acc v -> if v = 0 then acc else acc + 1) 0 x
    else
    if canBeDone input then
      begin
        let states = Hashtbl.create 1
        and que = ref (Queue.create ())
        and res = ref (-1)
        and startState = Array.make n 0 in
        Queue.push (startState, 0) !que;
        while (not (Queue.is_empty !que)) && !res = - 1 do
          res := backtrack states que x y n
        done;
        !res;
      end
    else -1;;