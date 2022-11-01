open Bogue
module W = Widget
module L = Layout
module E = Tsdl.Sdl.Event

let splash_screen = "snoke_title.png"
let quit _ _ _ = raise Bogue.Exit

let halloween = false

let pumkin (w,h) images_dir =
  let img = W.image (Filename.concat images_dir "pumkin2.png") in
  let pumkin = L.resident ~x:0 ~y:0 img in
  L.hide ~duration:0 pumkin;
  Timeout.add 3000 (fun () ->
      print_endline "Boooo";
      W.update img;
      L.show pumkin;
      L.zoom ~duration:4000 ~from_factor:0.1 ~to_factor:7. pumkin;
      L.oscillate ~duration:3000 ~frequency:5. (w/2) pumkin;
      L.animate_y pumkin (Avar.oscillate h 0 ~frequency:3. ~duration:3000);
      Timeout.add 4000 (fun () ->
          L.animate_y pumkin (Avar.fromto ~duration:1000 (L.ypos pumkin) (-h));
          L.rotate ~duration:1000 ~angle:360. pumkin;
          L.fade_out ~duration:1000 ~hide:true pumkin)
      |> ignore)
  |> ignore;
  pumkin


let splash ?w ?h images_dir =
  let img = W.image ?w ?h (Filename.concat images_dir splash_screen) in
  let c = W.connect_main img img quit E.[ key_down ] in
  W.add_connection img c;
  let background =
    L.style_bg
    @@ Style.of_bg
      (Style.gradient Draw.[ opaque white; opaque (find_color "skyblue") ])
  in
  let room = L.resident ~background img in
  L.zoom ~duration:1000 ~from_factor:10. ~to_factor:1. room;
  L.hide ~duration:0 room;
  Timeout.add 100 (fun () -> L.show room) |> ignore;
  if halloween then
    let s = L.get_size room in
    L.superpose [room; pumkin s images_dir]
  else room
