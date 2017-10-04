open Printf

let the_time_file = "/tmp/ocanren_time"

let wrap_run num rel ?(n= -1) ~reifier ~inj ~verbose onVerbose =
  MiniKanren.run num rel
    (fun s ->
      MiniKanren.Stream.take ~n s |>
      List.iter (fun r ->
        let term = r#reify reifier ~inj in
        if verbose then onVerbose term else ()
        )
      )

let time f =
  let t = Mtime_clock.counter () in
  let _res = f () in
  (Mtime_clock.count t |> Mtime.Span.to_s)
;;

let wrap (do_measure : verbose:bool -> unit) =
  try ignore (Sys.getenv "DONT_RUN_CHEZ");
      (* warmup *)
      let () = do_measure ~verbose:false in

      (* do benchmarking *)
      let n = 10 in
      let acc = ref 0. in
      for i=1 to n do
        let () = Gc.compact () in
        let () = Gc.full_major () in
        acc := !acc +. (time @@ fun () -> do_measure ~verbose:false);
      done;
      let ans =  (!acc /. (float_of_int n)) in
      let (_:int) = Sys.command @@ sprintf "echo %f > %s" ans the_time_file in
      Printf.printf "%f\n" ans

  with Not_found ->
    (* do normal run *)
    let () = do_measure ~verbose:true in
    (* let () = MiniKanren.report_counters () in *)
    ()
