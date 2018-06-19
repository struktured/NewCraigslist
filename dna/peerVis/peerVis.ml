open Hc
module Z =
struct let name = "peerVis" end (* TODO add this instantiation arg to builder *)
module PeerLink =
struct
type t =
  {
    links : Link.t array [@bs.as "Links"]
  } [@@bs.deriving abstract]

  (* TODO - wrong: should be provided by B.Add *)
  include (
    Entry.Make(struct
      let name = "peerLink"
      type nonrec t = t
    end) : Entry.S with type t := t
  )
end


module Genesis :
sig
  val genesis : unit -> bool
end = struct
  let genesis () =
    let hash = PeerLink.commit
        (PeerLink.t
           ~links:
             [|
               (Link.t
                  ~base:App.DNA.hash
                  ~tag:(Some "peer")
                  ~to_:App.Key.hash
               )
             |]
        )
    in
    Js.log2 "genesis: hash=" hash;
    true
end

 
module Sendreceive = struct
  module T = struct
    type input = { msg:string } [@@deriving bs.abstract]
    type output = string [@@deriving bs.abstract]
    let receive (_:hashString) {msg} =
      Js.log2 "receive: " msg;
      msg
  end
  include Sendreceive.Make(T)
end

module ValidatePeerLink : Validate.S with type t = PeerLink.t = struct
  type t = PeerLink.t

  let validateCommit
      ~package:_ ~sources:_ _t = true
  let validatePut
       ~header:_ ~package:_ ~sources:_ _t = true
  let validateMod
       ~header:_
      ~replaces:_ ~package:_ ~sources:_ _t = true
  let validateDel
      ~hash:_ ~package:_ ~sources:_ = true
  let validateLink
       ~hash:_ ~package:_ ~sources:_ ~links:_ = true
  let validatePutPkg () = Js.Json.null
  let validateModPkg () = Js.Json.null
  let validateDelPkg () = Js.Json.null
  let validateLinkPkg () = Js.Json.null
end

module GetPeers = struct
module T = struct
  module Zome = Z
  let name = "getPeers"
  type input = unit
  type output = {me:bool;address:hashString} [@@deriving bs.abstract]
end
include Function.Make(T)
end

let getPeers() =
  let possiblePeers =
    Zome.GetLinks.getLinks
      ?options:None
      ~base:App.DNA.hash ~tag:"peer" in
      Belt_Array.keepMap possiblePeers
        (function
          | `Hash hash ->
            (try
               let _res = Sendreceive.send hash
                   Sendreceive.T.{msg="hi"} in
               Some
                 (GetPeers.T.
                    {me=hash = App.Key.hash;
                     address=hash
                    }
                 )
             with _ -> None
            )
          | `Packed _ -> assert false (* TODO make this impossible by virtue of type signatures *)
        )


module B = Zome.Builder()
module PeerLink' = B.Add(PeerLink)(ValidatePeerLink)

include B.Build(Genesis)(Sendreceive)
