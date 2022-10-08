(* Levels for the snake game *)
(* San Vu Ngoc, 2022 *)

type pos = int * int

type t = {
  id : int;
  target : pos list;
  (* The head of the list is the head of the snake shed skin. (0,0) is top
     left. *)
  fps : int;
  is_new : bool;
  completed : bool;
}

(* Some geometric tools to draw a level *)

let tranpose list = List.map (fun (x, y) -> (y, x)) list

let hflip list =
  let y0 = snd (List.hd list) in
  List.map (fun (x, y) -> (x, (2 * y0) - y)) list

(* Return a list of positions approximating the line from p0 to p1, end points
   included. First element is p0. *)
let rec line (x0, y0) (x1, y1) =
  (* We draw backwards from p1 to p0.  Hence, for convenience we assume x1 <=
     x0 *)
  if x0 < x1 then List.rev (line (x1, y1) (x0, y0))
  else if y0 < y1 then hflip (line (x0, y0) (x1, (2 * y0) - y1))
  else
    (* Now we have only two possible moves: right or down, we determine which
       one by computing the distance to the ideal line. *)
    let dx = x0 - x1 in
    let dy = y0 - y1 in
    let c = ((1 - (2 * x1)) * dy) - ((1 - (2 * y1)) * dx) in
    let rec loop list (x, y) =
      if (x, y) = (x0, y0) then list
      else
        let s = (2 * ((x * dy) - (y * dx))) + c in
        let p = if s < 0 then (x + 1, y) else (x, y + 1) in
        loop (p :: list) p
    in
    loop [ (x1, y1) ] (x1, y1)

(* We draw from the tail to the head. *)
let line_to p list =
  let p0 = List.hd list in
  List.rev_append (line p0 p) (List.tl list)

let translate (x, y) = List.map (fun (u, v) -> (u + x, v + y))

(* The levels *)

let level0 = [ (0, 3) ] |> line_to (0, 0) |> translate (20, 10)
let level1 = [ (1, 0) ] |> line_to (1, 3) |> line_to (0, 3) |> translate (19, 4)
let level2 = [ (1, 3) ] |> line_to (1, 0) |> line_to (0, 0) |> translate (22, 0)

let level3 =
  [ (6, 1) ]
  |> line_to (1, 1)
  |> line_to (1, 0)
  |> line_to (0, 0)
  |> translate (15, 15)

let level4 =
  let size = 5 in
  let l = Array.init size (fun i -> i) |> Array.to_list in
  List.map (fun x -> (x + 1, 0)) l
  |> List.rev_append (List.map (fun y -> (0, y)) l)
  |> List.rev_append (List.map (fun x -> (x, size)) l)
  |> List.rev_append (List.map (fun y -> (size, size - y)) l)
  |> translate (10, 10)

let level5 =
  [ (5, 2) ]
  |> line_to (2, 2)
  |> line_to (2, 1)
  |> line_to (0, 0)
  |> translate (5, 10)

let level6 =
  [ (4, 3) ]
  |> line_to (3, 3)
  |> line_to (3, 2)
  |> line_to (0, 0)
  |> translate (15, 10)

let level7 =
  [ (3, 4) ]
  |> line_to (0, 4)
  |> line_to (0, 0)
  |> line_to (4, 0)
  |> line_to (4, 7)
  |> translate (20, 10)

let level8 =
  [ (3, 4) ]
  |> line_to (0, 4)
  |> line_to (0, 0)
  |> line_to (4, 0)
  |> line_to (4, 5)
  |> line_to (3, 5)
  |> tranpose
  |> translate (9, 6)

let level9 =
  [ (0, 6) ]
  |> line_to (6, 6)
  |> line_to (6, 0)
  |> line_to (1, 0)
  |> line_to (1, 4)
  |> line_to (4, 4)
  |> line_to (4, 2)
  |> line_to (3, 2)
  |> translate (12, 10)

let level10 = List.rev level9

let levels =
  [|
    level0;
    level1;
    level2;
    level3;
    level4;
    level5;
    level6;
    level7;
    level8;
    level9;
    level10;
  |]

let get id =
  let n = Array.length levels in
  let i = id mod n in
  let target = levels.(i) in
  let fps = 7 + (2 * (id / n)) in
  { id; target; fps; is_new = true; completed = false }

let complete t = { t with completed = true }
let fsp t = t.fps
let is_completed t = t.completed
let target t = t.target
let id t = t.id
let is_new t = t.is_new
let start t = { t with is_new = false }
