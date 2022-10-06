(* Levels for the snake game *)
(* San Vu Ngoc, 2022 *)

type pos = int * int

type t = {
  id : int;
  target : pos list;
  fps : int;
  is_new : bool;
  completed : bool }

let translate (x,y) =
  List.map (fun (u,v) -> (u+x, v+y))

let level0 = [
  (0,0);
  (0,1);
  (0,2);
  (0,3) ] |> translate (20,10)

let level1 = [
  (0,3);
  (1,3);
  (1,2);
  (1,1);
  (1,0) ] |> translate (19,4)

let level2 = [
  (0,0);
  (1,0);
  (1,1);
  (1,2);
  (1,3) ] |> translate (22,0)

let level3 = [
  (0,0); (1,0);
  (1,1); (2,1);
  (3,1);
  (4,1);
  (5,1);
  (6,1) ] |> translate (15,15)

let level4 =
  let size = 5 in
  let l = Array.init size (fun i -> i) |> Array.to_list in
  List.map (fun x -> (x+1,0)) l
  |> List.rev_append (List.map (fun y -> (0,y)) l)
  |> List.rev_append (List.map (fun x -> (x,size)) l)
  |> List.rev_append (List.map (fun y -> (size,size-y)) l)
  |> translate (10,10)

let level5 = [
  (0,0); (1,0);
  (1,1); (2,1);
  (2,2); (3,2);
  (4,2); (5,2) ] |> translate (5,10)

let level6 = [
  (0,0); (1,0);
  (1,1); (2,1);
  (2,2); (3,2);
  (3,3); (4,3) ] |> translate (15,10)

let levels = [|
  level0;
  level1;
  level2;
  level3;
  level4;
  level5;
  level6 |]

let get id =
  let n = Array.length levels in
  let i = id mod n in
  let target = levels.(i) in
  let fps = 7 + 2 * (id / n) in
  { id; target; fps; is_new = true; completed = false }

let complete t =
  { t with completed = true }

let fsp t = t.fps

let is_completed t =
  t.completed

let target t =
  t.target

let id t =
  t.id

let is_new t =
  t.is_new

let start t =
  { t with is_new = false }
