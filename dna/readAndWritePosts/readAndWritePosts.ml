open Hc
open Printf

(* CONSTANTS *)
let cityLinks = "cityLinks"
let city = "city"
let postData = "postData"
let category = "category"
let cityAndCategory = "cityAndCat"
let postsByUser = "postsByUser"
let postsByCity = "postsByCity"
let postsByCategory = "postsByCategory"

module Z : Zome.S0 =
struct
  let name = "readAndWritePosts"
end


module Builder = Zome.Builder (Z)

(**  Entries **)

module PostData =
  struct
    include Post_schema
    module Validate = Validate.Accept_all(Post_schema)
    include (Builder.Entry0
               (Post_schema)
               (Validate) :
               Entry.S with type t := t)
end

module StringEntry =
struct
  module type S0 =
  sig
    include Entry.S0 with type t = string
  end

  module type S =
  sig
    include Entry.S with type t = string
  end

  module Make(E:S0) : S with type t = string =
  struct
    include Builder.Entry0(E)(Validate.Accept_all(E))
  end

end

module City =
  StringEntry.Make
    (struct
      let name = "city"
      type t = string
    end
    )

module Category =
  StringEntry.Make
    (struct
      let name = "category"
      type t = string
    end
    )

module CityAndCat =
  StringEntry.Make
    (struct
      let name = "cityAndCat"
      type t = string
    end
    )

module CityLinks =
  struct
      module T =
        struct
          let name = "cityLinks"
          type t = Links.t
        end
  include Builder.Entry0(T)(Validate.Accept_all(T))
end
(**
 * @param key is the tag the link is associated with
 * @param link is the plaintext name whose existence we are verifying
 * If the link does not exist in the HC then it is created
 **)
let linkCheck
    (module E:StringEntry.S) (link:string) =
  Js.log2 "linkCheck: for link " link;
  debug(sprintf "linkCheck for link %s" link);
  let hashedKeyAndLink =
    E.makeHash link in
  Js.log2 "linkCheck: hash=" hashedKeyAndLink;
   (**
   * This block takes care of the case where duplicate entry has been deleted
   * and is then re-linked. get returns an error that the link has been deleted
   * This may be sort of an edge case, although I could see how someone might
   * add, remove, then re-add the same post (uniqueness is determined by link values, not timestamp)
   **)

  let _link =
    try
      E.get ?options:None hashedKeyAndLink
    with e ->
      debug e;
      E.commit link |> fun hashString ->
      E.get ?options:None hashString
  in
  match E.get hashedKeyAndLink with
  | None ->
    debug (link ^ " does not exist");
    (try
      let _hashString = E.commit link in
      debug ("Created " ^ link)
     with e ->
       debug (e)
    )
  | Some _ -> ()


(**
 * @param data is the post as a JSON object
 * @returns hash of the data from the commit
 * creates a post linked to:
 * - agent hash
 * - city provided in the data
 * - category provided in the data
 * - city and category provided in the data *)
 let writePost data =
   let hash =
  try
    Some (PostData.commit data)
  with e ->
    debug
      (Printf.sprintf "Error writing %s %s"
         (Js.Json.stringify (PostData.toJson data))
         (Printexc.to_string e)
      );
    None
   in
   let me = App0.Agent.hash in
   let city = PostData.city data in
   let category = PostData.category data in
   let cityAndCat = city ^ category in
   debug(sprintf "writePost: making hash for %s" cityAndCat);
   let cityAndCatHash = CityAndCat.makeHash cityAndCat in
  (* Check and create any links that may not yet exist *)
   linkCheck (module CityAndCat) cityAndCat;
   linkCheck (module City) city;
   linkCheck (module Category) category;
  match hash with None -> failwith "no hash" | Some hash ->
  try
    CityLinks.commit
      (
        Links.t
      [|
        Links.Link.t
          ~base:me
          ~link:hash
          ~tag:postsByUser
          ~linkAction:System.LinkAction.add
           ();
        Links.Link.t
          ~base:(City.makeHash city)
          ~link:hash
          ~tag:postsByCity
          ();
        Links.Link.t
          ~base:(Category.makeHash category)
          ~link:hash
          ~tag:postsByCategory
          ~linkAction:System.LinkAction.add
           ();
        Links.Link.t
          ~base:cityAndCatHash
          ~link:hash
          ~tag:cityAndCategory
          ~linkAction:System.LinkAction.add
           ()
      |]
      ) |> fun _ -> Js.Null.return hash
  with e ->
    debug (sprintf "Error committing links %s" (Printexc.to_string e));
    Js.Null.empty

(**
 * @param hash is hashedLink we are retrieving (ie. target value)
 * @param tag is the tag given when the link was created (ie. the relationship btwn link and base)
 * @returns an array of entries matching the hash given *)
let retrieveLinks hash tag :
  PostData.t array =
  (try
     Links.get ~tag hash
       ~options:
         (Links.Options.t
            ~load:true
            ~statusMask:System.Status.live
         )
   with e ->
     debug("Unable to retrieve links " ^ (Printexc.to_string e));
     [||]
  ) |>
  Links.unpack (module PostData) |> fun arr ->
  Belt_Array.forEach arr (fun t ->
      Js.log2 "postData array elem:" (Js.Json.stringify (PostData.toJson t.entry)));
  Belt_Array.map arr (fun {entry} -> entry)


(**
 * @returns all the posts of the current user
*)

let readYourPosts () =
  retrieveLinks App0.Agent.hash postsByUser

(**
 * @param city name
 * @returns all the posts for the given city
 *)

let readPostsByCity city =
  retrieveLinks (City.makeHash city) postsByCity

(**
 * @param category name
 * @returns all the posts for the given category
 *)
let readPostsByCategory category =
  retrieveLinks (Category.makeHash category) postsByCategory

(**
 * @param data is a JSON object: {"city":<name_of_city>, "category": <name_of_category}
 * @returns all the posts for the given city and category
 *)

module CityCat = struct
  type t = {city:string;category:string} [@@bs.deriving abstract]
end

let readPostsByCityAndCategory (data:CityCat.t) =
  let hashedCat = CityAndCat.makeHash
      (CityCat.city data ^ CityCat.category data) in
  retrieveLinks hashedCat cityAndCategory

(**
 * @param data is a JSON object {"post":{<new data>}, "oldHash": "<previous_hash>"}
 * @returns hash of the updated post *)

type edit = {post:PostData.t;oldHash:PostData.t HashString.t} [@@bs.deriving abstract]

let editPost edit =
  try
    PostData.update (post edit) (oldHash edit)
  with e ->
    debug("Update not made: " ^ (Printexc.to_string e));
    (oldHash edit)

(**
 * @param postHash is the hash of the post to delete
 * @returns true if the deletion was successful and false otherwise
 *)
let rec removePost postHash =
  match deleteLinks postHash with
  | true ->
    deletePost postHash
  | false -> false
(**
 * @param postHash is the hash of the post to delete
 * @returns true if the deletion was successful and false otherwise
 *)
and deletePost postHash =
  let message = postHash ^ " deleted by " ^ App0.Agent.hash in
  try
    let _hash = PostData.remove ~message postHash in true
  with _e ->
   (*debug(postHash + " not deleted: " + exception);*)
    false
(**
 * @param postHash is the hash of the post to delete
 * @returns true if the links were deleted and false otherwise
 *)
and deleteLinks postHash =
  let me = App0.Agent.hash in
  match PostData.get postHash with
  | None -> false
  | Some data ->
    let cityAndCatHash =
      CityAndCat.makeHash (PostData.city data ^ PostData.category data) in
  try
    CityLinks.commit
      (Links.
         (t
            [|
              Link.t
                ~base:me
                ~link:postHash
                ~tag:postsByUser
                ~linkAction:System.LinkAction.del
                ()
              ;
              Link.t
                ~base:(City.makeHash (PostData.city data))
                ~link:postHash
                ~tag:postsByCity
                ~linkAction:System.LinkAction.del
                ()
              ;
              Link.t
                ~base:(Category.makeHash (PostData.category data))
                ~link:postHash
                ~tag:postsByCategory
                ~linkAction:System.LinkAction.del
                ()
              ;
              Link.t
                ~base:cityAndCatHash
                ~link:postHash
                ~tag:cityAndCategory
                ~linkAction:System.LinkAction.del
                ()
            |]
         )
      ) |> fun _hash -> true
  with e ->
    debug("Links not deleted: " ^ (Printexc.to_string e));
    false

let readPost hash =
  (* get returns entry corresponding to the hash
     or a HashNotFound message *)
  PostData.get hash ~options:(Entry.GetOptions.t ~local:true ()) |> Js.Null.fromOption


include Builder.Build(Genesis.Success)(Sendreceive.Unit)

