open Bogue
module W = Widget
module L = Layout
module E = Tsdl.Sdl.Event

let splash_screen = "snoke_title.png"

let quit _ _ _ =
  raise Bogue.Exit

let splash ?w ?h images_dir =
  let img = W.image ?w ?h (Filename.concat images_dir splash_screen) in
  let c = W.connect_main img img quit E.[key_down] in
  W.add_connection img c;
  let background = L.style_bg @@ Style.of_bg
      (Style.gradient Draw.[opaque white; opaque (find_color "skyblue")]) in
  let room = L.resident ~background img in
  L.zoom ~duration:1000 ~from_factor:10. ~to_factor:1. room;
  L.hide ~duration:0 room;
  Timeout.add 100 (fun () -> L.show room) |> ignore;
  room
