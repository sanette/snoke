(* Snóke, a BOGUE version of the snake game, with new ideas!

   San Vu Ngoc, 2022

   Adapted and enhanced from an ocaml/javascript version by Florent Monnier,
   http://decapode314.free.fr/re/tut/ocaml-re-tut.html

   The rules of the game:

   In order to complete a level, you need to cover exactly the grey snake (the
   "target"). Hence you need first to eat the exact amout of fruit needed to
   have the targetted size.

   Bananas give you more points and make you slide faster, except when you reach
   the size of the target.
*)

open Printf
open Tsdl
open Bogue
module W = Widget
module L = Layout
module E = Sdl.Event

(* Global variables *)
let images_dir = "images"
let sounds_dir = "sounds"
let width, height = (32, 24)
let scale = 20
let smooth = true
let print_messages = false
let fps_boost = 10
let bonus = 4 (* optimal path score bonus *)
let max_lives = 3

let prefix =
  match Theme.find_share "snoke" "SnakeChan-MMoJ.ttf" with
  | None ->
      print_endline "Cannot find share directory!";
      "."
  | Some path -> path

let ( // ) = Filename.concat

(* Overrides some of Bogue's theme variables *)
let () =
  Theme.set_integer_scale true;
  let snake_font = prefix // "SnakeChan-MMoJ.ttf" in
  Theme.set_label_font snake_font;
  Theme.set_text_font snake_font;
  Draw.(set_text_color (find_color "azure"))

type pos = int * int
type direction = Left | Right | Up | Down
type fruit = Apple | Banana | Bonus

type game_state = {
  pos_snake : pos;
  seg_snake : pos list;
  dir_snake : direction;
  fruit : fruit;
  pos_fruit : pos;
  distance : int;
  has_bonus : bool;
  steps : int;
  game_over : bool;
  fps : int;
  score : int;
  level : Levels.t;
  full_size : bool;
  paused : bool;
  lives : int;
}

type images = {
  snake_body : W.t;
  snake_head : W.t;
  snake_tail : W.t;
  snake_turn_pos : W.t;
  snake_turn_neg : W.t;
  apple : W.t;
  banana : W.t;
  bonus : W.t;
}

type sounds = {
  eat : Mixer.sound;
  right : Mixer.sound;
  up : Mixer.sound;
  left : Mixer.sound;
  down : Mixer.sound;
  over : Mixer.sound;
  fast : Mixer.sound;
  completed : Mixer.sound;
  new_level : Mixer.sound;
  yeah : Mixer.sound;
  great : Mixer.sound;
}

type cell = L.t (* a [cell] is a layout of size scale x scale *)

type score_board = {
  score : W.t;
  length : W.t;
  target_length : W.t;
  new_message : ?delay:int -> string -> unit;
  set_lives : int -> unit;
  layout : L.t;
}

type area = {
  snake : L.t;
  target : L.t;
  screen : L.t;
  fruit : cell;
  images : images;
  target_images : images;
  sounds : sounds;
  score_board : score_board;
  game_bg : L.background option;
}

(* Manhattan distance (L¹) *)
let dist (x, y) (x1, y1) = abs (x - x1) + abs (y - y1)

let vec_dir (x1, y1) (x2, y2) =
  match (x2 - x1, y2 - y1) with
  | 1, 0 -> Right
  | 0, -1 -> Up
  | -1, 0 -> Left
  | 0, 1 -> Down
  | _ -> invalid_arg "[vec_dir] wrong vector"

(* Direction of the first element (head) of the list *)
let dir_seg = function
  | a :: b :: _ -> vec_dir b a
  | _ -> invalid_arg "[dir_seg] list too short"

(* Positive means counterclockwise *)
let positive_turn = function
  | Right, Up | Down, Right | Left, Down | Up, Left -> true
  | _ -> false

let angle_from_dir = function
  | Left -> 180.
  | Right -> 0.
  | Up -> 270.
  | Down -> 90.

let images_dir = prefix // images_dir
let sounds_dir = prefix // sounds_dir
let load_img file = W.image ~w:scale ~h:scale (images_dir // file)

let load_images () =
  {
    snake_body = load_img "snake-body.png";
    snake_head = load_img "snake-head.png";
    snake_tail = load_img "snake-tail.png";
    snake_turn_pos = load_img "snake-turn-pos.png";
    snake_turn_neg = load_img "snake-turn-neg.png";
    apple = load_img "pumkin.png";
    banana = load_img "banana.png";
    bonus = load_img "bonus.png";
  }

let load_target_images () =
  {
    snake_body = load_img "snake-body-grey.png";
    snake_head = load_img "snake-head-grey.png";
    snake_tail = load_img "snake-tail-grey.png";
    snake_turn_pos = load_img "snake-turn-pos-grey.png";
    snake_turn_neg = load_img "snake-turn-neg-grey.png";
    apple = W.empty ~w:scale ~h:scale ();
    banana = W.empty ~w:scale ~h:scale ();
    bonus = W.empty ~w:scale ~h:scale ();
  }

let get_turn_image old_dir new_dir images =
  if positive_turn (old_dir, new_dir) then images.snake_turn_pos
  else images.snake_turn_neg

(* Warning, if we indicate a direction in [make_cell], the rotation is permanent
   to the new image, contrary to using L.rotate. *)
let make_cell ?direction image (x, y) =
  let rotate = Option.map angle_from_dir direction in
  let cell =
    L.resident ~x:(x * scale) ~y:(y * scale) (W.image_copy ?rotate image)
  in
  (* Option.iter (rotate_cell cell) direction; *)
  assert (L.height cell = scale);
  cell

let move_cell cell (x, y) =
  L.setx cell (x * scale);
  L.sety cell (y * scale)

let fruit_layout images fruit (x, y) =
  let image =
    match fruit with
    | Apple -> images.apple
    | Banana -> images.banana
    | Bonus -> images.bonus
  in
  L.resident ~x:(x * scale) ~y:(y * scale) image

let load_sound, play_sound =
  let devname = Mixer.init () in
  let mixer = Mixer.create_mixer devname in
  Mixer.unpause mixer;
  ( (fun ?(volume = 0.5) file ->
      let c = Mixer.load_chunk mixer (sounds_dir // file) in
      Mixer.change_volume volume c;
      c),
    fun sound -> ignore @@ Mixer.play_chunk mixer sound )

let load_sounds () =
  {
    eat = load_sound ~volume:0.4 "crunch.wav";
    right = load_sound ~volume:0.2 "do.wav";
    up = load_sound ~volume:0.2 "sol.wav";
    left = load_sound ~volume:0.2 "fa.wav";
    down = load_sound ~volume:0.2 "re.wav";
    over = load_sound "over.wav";
    fast = load_sound ~volume:0.4 "fast.wav";
    completed = load_sound "completed.wav";
    new_level = load_sound "new_level.wav";
    yeah = load_sound ~volume:0.2 "oh_yeah.wav";
    great = load_sound ~volume:0.2 "great.wav";
  }

let fruit_sound sounds = function
  | Apple -> sounds.eat
  | Banana -> sounds.fast
  | Bonus -> sounds.fast

let play_sounds sounds old_state new_state =
  if Levels.is_new new_state.level then
    if not (Levels.is_new old_state.level) then play_sound sounds.new_level
    else () (* and nothing else *)
  else if new_state.game_over then
    if not old_state.game_over then play_sound sounds.over
    else () (* nothing else *)
  else (
    (* Eating a fruit *)
    if old_state.pos_fruit <> new_state.pos_fruit then
      if new_state.full_size then play_sound sounds.yeah
      else if new_state.has_bonus then play_sound sounds.great
      else play_sound (fruit_sound sounds old_state.fruit);

    (* Changing direction *)
    if old_state.dir_snake <> new_state.dir_snake then
      let sound =
        match new_state.dir_snake with
        | Right -> sounds.right
        | Up -> sounds.up
        | Left -> sounds.left
        | Down -> sounds.down
      in
      play_sound sound
    else ();

    if (not old_state.level.completed) && new_state.level.completed then
      play_sound sounds.completed)

(* The layout that holds the number of remaining lives *)
let make_lives n =
  let fg = Draw.(opaque red) in
  let rec loop list i =
    if i = n then list
    else loop (L.resident (W.icon ~size:13 ~fg "heart") :: list) (i + 1)
  in
  let list = loop [] 0 in
  let layouts = Array.of_list list in
  let set x =
    for i = 0 to n - 1 do
      if i < x then L.show layouts.(i) else L.hide layouts.(i)
    done
  in
  (L.flat ~sep:0 ~margins:0 list, set)

let make_score_board () =
  let background = L.color_bg Draw.(opaque black) in
  let fg = Draw.(opaque green) in
  let label_score = W.label ~fg ~align:Draw.Max "Score:" in
  let label_length = W.label ~fg ~align:Draw.Max "Length:" in
  let score = W.label ~fg ~align:Draw.Min "0000" in
  let length = W.label ~fg ~align:Draw.Max "00" in
  let target_length = W.label ~fg ~align:Draw.Min "/00" in
  let messages =
    W.label ~fg:Draw.(opaque (find_color "azure")) "Welcome to snoke!"
  in
  let w = 5 * width * scale / 16 in
  let msg = L.resident ~w messages in
  let a = L.flat_of_w [ label_score; score ] in
  let b = L.flat_of_w [ label_length; length; target_length ] in
  let lives, set_lives = make_lives 3 in
  let layout = L.flat ~margins:2 ~background [ lives; a; msg; b ] in
  L.set_width layout (width * scale);
  let new_message =
    let timeout = ref None in
    fun ?delay text ->
      Option.iter Timeout.cancel !timeout;
      let f () =
        if print_messages then print_endline text;
        W.set_text messages text;
        L.fade_in msg
        (* L.slide_in ~dst:layout msg *)
      in
      match delay with
      | None ->
          timeout := None;
          f ()
      | Some t -> timeout := Some (Timeout.add t f)
  in
  { score; length; target_length; new_message; set_lives; layout }

(* Create the snake from the list of positions *)
let build_snake images seg_snake =
  let pos = List.hd seg_snake in
  let direction = dir_seg seg_snake in
  let head = make_cell ~direction images.snake_head pos in
  let rec loop direction snake_rev = function
    | [] -> invalid_arg "[build_snake] list too short"
    | [ p ] ->
        let tail = make_cell ~direction images.snake_tail p in
        List.rev (tail :: snake_rev)
    | p :: rest as list ->
        let new_dir = dir_seg list in
        let image =
          if new_dir = direction then images.snake_body
          else get_turn_image new_dir direction images
        in
        let cell = make_cell ~direction:new_dir image p in
        loop new_dir (cell :: snake_rev) rest
  in
  loop direction [ head ] (List.tl seg_snake)

let make_area () =
  let w, h = (scale * width, scale * height) in
  let images = load_images () in
  let target_images = load_target_images () in
  {
    snake = L.empty ~name:"snake" ~w ~h ();
    target = L.empty ~name:"snake" ~w ~h ();
    screen = L.resident ~name:"screen" (W.empty ~w ~h ());
    fruit = L.empty ~w:scale ~h:scale ();
    images;
    target_images;
    sounds = load_sounds ();
    score_board = make_score_board ();
    game_bg = None;
  }

(* Smoothing animations: *)
(* (can be disabled by setting smooth=false) *)

let fromto = Avar.fromto_unif

(* Apply smooth rotation from the *previous* rotation to smoothly arrive at the
   *current* rotation. *)
let smooth_rotate_cell duration from_dir to_dir cell =
  let angle = if positive_turn (from_dir, to_dir) then -90. else 90. in
  L.rotate ~duration ~angle ~from_angle:(-.angle) cell

(* move smoothly from p0 to (x,y) *)
let smooth_move_cell duration cell p0 (x, y) =
  move_cell cell p0;
  L.animate_x cell (fromto ~duration (L.xpos cell) (x * scale));
  L.animate_y cell (fromto ~duration (L.ypos cell) (y * scale))

let smooth_head duration head = function
  | a :: b :: c :: _ ->
      smooth_move_cell duration head b a;
      if vec_dir b a <> vec_dir c b then
        smooth_rotate_cell duration (vec_dir c b) (vec_dir b a) head
  | _ -> invalid_arg "[smooth_head] snake too short"

(* We need to animate the second segment after the head, otherwise it will show
   up behind the head the the latter is animated. *)
let smooth_second duration second = function
  | a :: b :: _ -> (
      match vec_dir b a with
      | Right -> L.animate_w second (fromto ~duration 1 (scale + 1))
      | Left ->
          let x, y = a in
          smooth_move_cell duration second (x + 1, y) a;
          L.animate_w second (fromto ~duration 1 (scale + 1))
      | Down -> L.animate_h second (fromto ~duration 1 (scale + 1))
      | Up ->
          let x, y = a in
          smooth_move_cell duration second (x, y + 1) a;
          L.animate_h second (fromto ~duration 1 (scale + 1)))
  | _ -> invalid_arg "[smooth_second] snake too short"

let smooth_tail area duration tail seg_snake old_pos snake =
  match List.rev seg_snake with
  | a :: b :: _ ->
      if a <> old_pos then
        let old_dir, new_dir = (vec_dir old_pos a, vec_dir a b) in
        if new_dir = old_dir then begin
          match new_dir with
          | Right ->
              L.animate_w tail (fromto ~duration (2 * scale) scale);
              smooth_move_cell duration tail old_pos a
          | Left -> L.animate_w tail (fromto ~duration (2 * scale) scale)
          | Up -> L.animate_h tail (fromto ~duration (2 * scale) scale)
          | Down ->
              L.animate_h tail (fromto ~duration (2 * scale) scale);
              smooth_move_cell duration tail old_pos a
        end
        else
          (* We revert the tail to the old direction.  *)
          let tail =
            make_cell ~direction:old_dir area.images.snake_tail old_pos
          in
          let () =
            match old_dir with
            | Left -> L.animate_w tail (fromto ~duration scale 0)
            | Up -> L.animate_h tail (fromto ~duration scale 0)
            | Right ->
                L.animate_w tail (fromto ~duration scale 0);
                smooth_move_cell duration tail old_pos a
            | Down ->
                L.animate_h tail (fromto ~duration scale 0);
                smooth_move_cell duration tail old_pos a
          in
          (* And add the "turn" element back, too.  *)
          let last_turn =
            make_cell ~direction:old_dir
              (get_turn_image old_dir new_dir area.images)
              a
          in
          L.set_rooms ~sync:false area.snake
            (tail :: last_turn :: List.tl (List.rev snake))
  | _ -> invalid_arg "[smooth_tail] snake too short"

let smooth_snake area duration seg_snake old_tail snake =
  let head = List.hd snake in
  smooth_head duration head seg_snake;
  let second = List.hd (List.tl snake) in
  smooth_second duration second (List.tl seg_snake);
  let tail = List.hd (List.rev snake) in
  smooth_tail area duration tail seg_snake old_tail snake

let apple_messages =
  [|
    "You love pumkins";
    "Yummy";
    "Crunch";
    "Good for you";
    "Still hungry!";
    "Eating for your life";
    "Bon appetit ";
    "Happy Halloween"
  |]

let fruit_message score = function
  | Apple ->
      if score < 2 then apple_messages.(0)
      else apple_messages.(Random.int (Array.length apple_messages))
  | Banana -> "Banana power!"
  | Bonus -> "Bonus!"

let gradient_bg color1 color2 =
  L.style_bg Style.(of_bg @@ gradient ~angle:45. [ color1; color2 ])

let lives_to_string x = sprintf "%u %s" x (if x > 1 then "lives" else "life")

let update_area area old_state state =
  if state.game_over && not old_state.game_over then begin
    let bg = gradient_bg Draw.(transp yellow) Draw.(transp red) in
    L.set_background area.screen (Some bg);
    L.oscillate ~duration:200 ~frequency:10. 5 (L.top_house area.snake);
    L.rotate ~duration:500 ~from_angle:0. ~angle:360. area.score_board.layout;
    area.score_board.set_lives (state.lives - 1);
    let delay =
      if List.compare_lengths state.seg_snake state.level.target > 0 then begin
        area.score_board.new_message "You ate too much!";
        Some 2000
      end
      else None
    in
    let msg =
      if state.lives = 1 then "Game over!"
      else sprintf "%s remaining" (lives_to_string (state.lives - 1))
    in
    area.score_board.new_message ?delay msg
  end;

  if
    Levels.is_completed state.level && not (Levels.is_completed old_state.level)
  then begin
    let bg = gradient_bg Draw.(transp white) Draw.(transp green) in
    L.set_background area.screen (Some bg);
    L.rotate ~angle:180. area.screen;
    area.score_board.new_message "Level completed!"
  end;

  (* When starting a new level *)
  if Levels.is_new state.level && not (Levels.is_new old_state.level) then begin
    L.set_background area.screen area.game_bg;
    L.set_rooms ~sync:false area.target
      (List.rev @@ build_snake area.target_images (Levels.target state.level));
    W.set_text area.score_board.target_length
      (sprintf "/ %i" (List.length (Levels.target state.level)));
    if state.score <> 0 || old_state.game_over then
      area.score_board.new_message
        ("Level " ^ string_of_int (Levels.id state.level + 1))
  end;

  let snake = build_snake area.images state.seg_snake in
  L.set_rooms ~sync:false area.snake (List.rev snake);

  (if
   smooth && not (state.paused && old_state.paused)
   (* If we have a new paused state we need to finish the animation
      anyway. *)
  then
   let old_tail = List.hd (List.rev old_state.seg_snake) in
   smooth_snake area (1000 / state.fps) state.seg_snake old_tail snake);

  if
    old_state.pos_fruit <> state.pos_fruit
    (* Only for optimization. We could set the fruit image etc. at each step. *)
  then begin
    let fruit = fruit_layout area.images state.fruit state.pos_fruit in
    if state.has_bonus then begin
      let bonus = fruit_layout area.images Bonus old_state.pos_fruit in
      L.set_rooms area.fruit [ fruit; bonus ];
      L.animate_y bonus (fromto ~duration:1000 (L.ypos bonus) (-8 * scale));
      L.fade_out ~duration:2000 ~hide:true bonus;
      L.zoom ~duration:1000 ~from_factor:1. ~to_factor:4. bonus
    end
    else L.set_rooms area.fruit [ fruit ];
    W.set_text area.score_board.score (string_of_int state.score);
    W.set_text area.score_board.length
      (string_of_int (List.length state.seg_snake));
    if (not (Levels.is_new state.level)) && not state.game_over then begin
      area.score_board.new_message
        (if state.full_size then "You are full!"
        else fruit_message old_state.score old_state.fruit);
      if not state.full_size then
        area.score_board.new_message ~delay:8000 "You are hungry"
    end
  end;

  if state.full_size && not old_state.full_size then begin
    L.set_background area.screen (Some (L.color_bg Draw.(opaque green)));
    Timeout.add 20 (fun () -> L.set_background area.screen area.game_bg)
    |> ignore
  end;

  if old_state.game_over && (not state.game_over) && old_state.lives = 1 then begin
    area.score_board.set_lives state.lives;
    Popup.yesno
      (Printf.sprintf
         "\n\n\
         \       Score: %u\n\
         \       Final level: %u\n\n\n\
         \          Start again?" old_state.score (old_state.level.id + 1))
      ~yes_action:(fun () ->
        L.claim_keyboard_focus area.screen;
        print_endline "Yes!")
      ~no_action:(fun () -> raise Bogue.Exit)
      (L.top_house area.screen)
  end

(* move_cell area.fruit state.pos_fruit *)

let rec pop = function
  | [ _ ] -> []
  | hd :: tl -> hd :: pop tl
  | [] -> invalid_arg "[pop]"

let new_fruit () = if Random.int 6 = 0 then Banana else Apple

let rec new_pos_fruit seg_snake target full_size =
  let new_pos = (Random.int width, Random.int height) in
  if List.mem new_pos seg_snake || (full_size && List.mem new_pos target) then
    new_pos_fruit seg_snake target full_size
  else new_pos

let create_state ?(score = 0) ~lives level =
  Printf.sprintf "Level = %i" level |> print_endline;
  let seg_snake = [ (5, 5); (4, 5); (3, 5) ] in

  let pos_snake = List.hd seg_snake in
  let pos_fruit = (10, 10) in
  let level = Levels.get level in
  {
    pos_snake;
    seg_snake;
    pos_fruit;
    distance = dist pos_fruit pos_snake;
    steps = 0;
    has_bonus = false;
    fruit = Apple;
    dir_snake = Right;
    game_over = false;
    fps = level.fps;
    score;
    level;
    full_size = false;
    paused = true;
    lives;
  }

(* The [update_state] function contains all the logic of the game. *)
let update_state req_dir key_pressed
    ({
       pos_snake;
       seg_snake;
       pos_fruit;
       fruit;
       dir_snake;
       distance;
       steps;
       has_bonus;
       game_over;
       fps;
       score;
       level;
       full_size;
       paused;
       lives;
     } as state) =
  (* Are we paused? *)
  let paused = if key_pressed = Sdl.K.space then false else paused in
  if paused then state (* Should we start a new level? *)
  else if level.completed then
    create_state ~score:state.score ~lives (Levels.id state.level + 1)
  else if game_over then
    if lives >= 2 (* restart the same level *) then
      create_state ~score ~lives:(lives - 1) level.id
    else create_state ~score:0 ~lives:max_lives 0
      (* Start again at level 0 *)
      (* Now we can respond to the recorded key press. *)
  else
    let new_dir_snake =
      match (dir_snake, req_dir) with
      | Left, Right | Right, Left | Up, Down | Down, Up -> dir_snake
      | _ -> req_dir
    in
    let pos_snake =
      let x, y = pos_snake in
      match new_dir_snake with
      | Left -> (x - 1, y)
      | Right -> (x + 1, y)
      | Up -> (x, y - 1)
      | Down -> (x, y + 1)
    in
    (* Do we hit the wall or ourself? *)
    let game_over =
      let x, y = pos_snake in
      List.mem pos_snake (pop seg_snake)
      || x < 0
      || y < 0
      || x >= width
      || y >= height
    in

    (* Are we on a fruit? *)
    let eating = pos_snake = pos_fruit in

    let seg_snake = pos_snake :: seg_snake in

    (* What happens when we eat a fruit: *)
    let full_size, game_over =
      if eating then
        let c = List.compare_lengths seg_snake level.target in
        (c = 0, c > 0)
      else (full_size, game_over)
    in

    let seg_snake, pos_fruit, fruit, distance, fps, score, has_bonus =
      if eating then begin
        print_endline "Miam";
        let fps =
          if fruit = Banana && not full_size then level.fps + fps_boost
          else level.fps
        in
        let score =
          if game_over then score
          else if fruit = Banana then score + 5
          else score + 1
        in
        let score, has_bonus =
          if steps + 1 = distance && not game_over then (score + bonus, true)
          else (score, false)
        in
        let new_pos_f = new_pos_fruit seg_snake level.target full_size in
        ( seg_snake,
          new_pos_f,
          new_fruit (),
          dist pos_fruit new_pos_f,
          fps,
          score,
          has_bonus )
      end
      else (pop seg_snake, pos_fruit, fruit, distance, fps, score, has_bonus)
    in

    let steps = if eating then 0 else steps + 1 in

    (* Did we complete the level? *)
    let level =
      if full_size && level.target = seg_snake then Levels.complete level
      else if Levels.is_new level then Levels.start level
      else level
    in

    let paused = game_over || Levels.is_completed level || paused in

    {
      pos_snake;
      seg_snake;
      pos_fruit;
      fruit;
      dir_snake = new_dir_snake;
      distance;
      steps;
      has_bonus;
      game_over;
      fps;
      score;
      level;
      full_size;
      paused;
      lives;
    }

let () =
  Random.self_init ();
  let initial_level = 6 in
  let lives = max_lives in
  let initial_state = create_state ~lives initial_level in
  let area = make_area () in
  let dummy_old_state =
    {
      initial_state with
      pos_fruit = (-1, -1);
      level = Levels.start initial_state.level;
    }
  in
  update_area area dummy_old_state initial_state;
  let state = ref initial_state in
  let req_dir = ref !state.dir_snake in
  let key_pressed = ref (-1) in

  let keychange_action _screen _none ev =
    (key_pressed := E.(get ev keyboard_keycode));
    req_dir :=
      match !key_pressed with
      | x when x = Sdl.K.left -> Left
      | x when x = Sdl.K.up -> Up
      | x when x = Sdl.K.right -> Right
      | x when x = Sdl.K.down -> Down
      | _ -> !state.dir_snake
  in

  (* The main game engine is here: the [one_step] function calls [update_state]
     at a regular rate, using Bogue's Timeout facility. *)
  let rec one_step () =
    let new_state = update_state !req_dir !key_pressed !state in
    play_sounds area.sounds !state new_state;
    update_area area !state new_state;
    state := new_state;
    key_pressed := -1;
    Update.push (L.widget area.screen);
    Timeout.add (1000 / new_state.fps) one_step |> ignore
  in

  (* Just for fun. *)
  let rec giggle_fruit () =
    if Random.int 4 = 0 then
      L.animate_angle area.fruit
        (Avar.oscillate ~duration:500 ~frequency:1. 30 0 |> Avar.apply float);
    Timeout.add 1000 giggle_fruit |> ignore
  in

  (* Now we construct the window. *)
  let background =
    L.style_bg
      Style.(of_bg @@ image_bg @@ Image.create (images_dir // "grass3.png"))
  in
  (* This background can be overrriden by area.game_bg *)
  let game_layout =
    L.superpose ~name:"layout" ~scale_content:false ~background
      [ area.screen; area.target; area.fruit; area.snake ]
  in

  (* The snake cells layouts will be added to the snake rooms. *)
  let layout =
    L.tower ~name:"snoke"
      ~background:(L.color_bg Draw.(opaque black))
      [ area.score_board.layout; game_layout ]
  in

  (* We register the key_down event *)
  let w = L.widget area.screen in
  let c = W.connect_main w w keychange_action E.[ key_down ] in

  (* Splash screen *)
  Popup.one_button ~button:"Play" ~dst:layout
    Splash.(splash ~w:(width * scale) ~h:(height * scale) images_dir)
    ~on_close:(fun () ->
      L.show layout;
      L.claim_keyboard_focus area.screen;
      area.score_board.new_message ~delay:3000 "Press Space to start";
      Sync.push one_step;
      Sync.push giggle_fruit);

  let board = Bogue.of_layout ~connections:[ c ] layout in

  Bogue.run board
