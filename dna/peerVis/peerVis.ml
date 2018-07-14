open Hc


module Z =
  struct
    let name = "peerVis"
  end (* TODO add this instantiation arg to builder *)

module PeerLink =
struct
  include Links

  (* TODO - wrong: should be provided by B.Add *)
  include (
    Entry.Make(struct
      let name = "peerLink"
      type nonrec t = t
    end) : Entry.S with type t := t
  )
end


module Genesis : Genesis.S = struct
  let genesis () =
    let hash = PeerLink.commit
        (PeerLink.
           (t
             [|
               (Link.t
                  ~base:(App.DNA.hash :> App.DNA.hash)
                  ~tag:"peer"
                  ~link:App.Key.hash
                  ()
               )
             |]
           )
        )
    in
    Js.log2 "genesis: hash=" hash;
    true
end


module Sendreceive = struct
  module T = struct
    type input = { msg:string } [@@deriving bs.abstract]
    type output = string [@@deriving bs.abstract]
    let receive (_:App.Agent.hash) {msg} =
      Js.log2 "receive: " msg;
      msg
  end
  include Sendreceive.Make(T)
end

module ValidatePeerLink : Validate.S with type t = PeerLink.t =
  Validate.Accept_all(PeerLink)

module GetPeers = struct
module T = struct
  module Zome = Z
  let name = "getPeers"
  type input = unit
  type output = {me:bool;address:[`Key] HashString.t} [@@deriving bs.abstract]
end
include Function.Make(T)
end

let getPeers() =
  let possiblePeers =
  Links.get
      ?options:None
      ~base:App.DNA.hash ~tag:"peer" in
      Belt_Array.keepMap possiblePeers
        (function
          | `Hash hash ->
            (try
               let hashString = (HashString.create hash :> App.Key.hash) in
               let _res =
                 Sendreceive.send hashString
                   Sendreceive.T.{msg="hi"} in
               Some
                 (GetPeers.T.
                    {me=HashString.equals App.Key.hash hashString;
                     address=hash
                    }
                 )
             with _ -> None
            )
          | `Packed _ -> assert false (* TODO make this impossible by virtue of type signatures *)
        )


module B = Zome.Builder(Z)
module PeerLink' = B.Entry(PeerLink)(ValidatePeerLink)

include B.Build(Genesis)(Sendreceive)
