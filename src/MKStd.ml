open MiniKanren

module Option = struct
  module T =
    struct
      type 'a t = 'a option
      let fmap f x = GT.(gmap option) f x
    end

  include T
  include Fmap1(T)

  let some x  = inj @@ distrib (Some x)
  let none () = inj @@ distrib None
end
