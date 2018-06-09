open Hc
type link_entry = {
  base : string [@bs.as "Base"];
  tag : string [@bs.as "Tag"];
  link : string [@bs.as "Link"]
} [@@bs.deriving abstract]

type peer_link =
  {links:link_entry array [@bs.as "Links"]} [@@bs.deriving abstract]

(*class peer_link2 =
  object
    method links:link_entry array [@bs.set]
  end
*)
module Callbacks : REQUIRED = struct

  let genesis () =
    let _hash = commit
      ~entry_type:"peerLink"
      ~entry:(peer_link
      ~links:
        [|
          (link_entry
             ~base:App.DNA.hash
             ~tag:"peer"
             ~link:App.Key.hash
          )
        |]
      ) in true

  let validateCommit
      ~entry_type:_ ~entry:_ ~package:_ ~sources:_ = true
  let validatePut
      ~entry_type:_ ~entry:_ ~header:_ ~package:_ ~sources:_ = true
  let validateMod
      ~entry_type:_ ~entry:_ ~header:_
      ~replaces:_ ~package:_ ~sources:_ = true
  let validateDel
      ~entry_type:_ ~hash:_ ~package:_ ~sources:_ = true
  let validateLink
      ~entry_type:_ ~hash:_
      ~links:_ ~package:_ ~sources:_ = true
  let validatePutPkg ~entry_type:_ : 'a Js.t = failwith "nyi"
  let validateModPkg  ~entry_type:_ : 'a Js.t = failwith "nyi"
  let validateDelPkg  ~entry_type:_ : 'a Js.t = failwith "nyi"

  let validateLinkPkg  ~entry_type:_ = failwith "nyi"

end

let getPeers() = failwith "nyi"

let possiblePeers =
  let possible_peers = get_links ~base:App.DNA.hash ~tag:"peer" in
  List.map possible_peers ~f:
    (fun p ->
       let res = send (* *)

  // try contacting each peer
  possiblePeers.forEach(function (p) {
    // try sending a message to peer
    try {
      var res = send(p.Hash, {msg:"hi"})
      // they're online
      peers.push({
        me: p.Hash === App.Key.Hash,
        address: p.Hash
      })
    } catch(e) {}
  })
  return peers
}

(*function receive(from, msg) {
  return "hi"
}*)
