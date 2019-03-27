open GT

open MiniKanren
open MiniKanrenStd

let rec foo e l =
   (l === nil ()) ||| (fresh (ls) ((l === e % ls) <&> (cont_delay @@ foo e ls)))

let main_goal q = foo !!"A" q <&> foo !!"B" q

open Tester

let run x = runR (List.reify MiniKanren.reify)
                 (show List.ground @@ show string)
                 (show List.logic (show logic @@ show string)) x


let () = run (-1) q qh ("example", fun q -> main_goal q)
