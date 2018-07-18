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
  let peerLink =
    PeerLink.
           (t
             [|
               (Link.t
                  ~base:App0.DNA.hash
                  ~tag:"peer"
                  ~link:App0.Key.hash
                  ()
               )
             |]
           ) in
    Js.log2 "genesis: peerLink="
      (PeerLink.toJson peerLink |> Js.Json.stringify);
    let hash = PeerLink.commit peerLink in
    Js.log2 "genesis: hash=" hash;
    true
end


module Sendreceive = struct
  module T = struct
    type input = { msg:string } [@@bs.deriving abstract]
    type output = string
    let receive (_:App0.Agent.hash) (m:input) =
      Js.log2 "receive: " (msg m);
      msg m
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
      ~base:App0.DNA.hash ~tag:"peer" in
      Belt_Array.keepMap possiblePeers
        (function
          | `Hash hash ->
            (try
               let hashString = (HashString.create hash :> App0.Key.hash) in
               let _res =
                 Sendreceive.send hashString
                   (Sendreceive.T.input ~msg:"hi") in
               Some
                 (GetPeers.T.
                    {me=HashString.equals App0.Key.hash hashString;
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
