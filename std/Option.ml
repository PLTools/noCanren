type nonrec 'a option = 'a option

module HO = struct
  include OCanren

  type 'a option = 'a OCanren.Std.Option.ground
  [@@deriving gt ~options:{ show; fmt; gmap }]

  type 'a option_logic = 'a OCanren.Std.Option.logic
  [@@deriving gt ~options:{ show; fmt; gmap }]

  type 'a option_injected = 'a OCanren.Std.Option.injected

  let option = OCanren.Std.Option.ground [@@deriving gt ~options:{ show; fmt; gmap }]
  let option_logic = OCanren.Std.Option.logic [@@deriving gt ~options:{ show; fmt; gmap }]
  let option_prj_exn = OCanren.Std.Option.prj_exn
  let option_reify = OCanren.Std.Option.reify
end

module FO = struct end
