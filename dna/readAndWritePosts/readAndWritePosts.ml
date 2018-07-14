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

  module T = struct
    let name = "postData"
    type t =
      {
        title:string;
        category:string;
        subcategory:string option;
        city:string;
        email:string;
        timestamp:int;
      } [@@deriving bs.abstract]
  end
  module Validate = Validate.Accept_all(T)
  include Builder.Entry0(T)(Validate)
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
      let name = cityAndCategory
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
  let hashedKeyAndLink =
    E.makeHash link in
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
   let me = App.Agent.hash in
   let cityAndCat = CityAndCat.makeHash
       (data.city ^ data.category) in
  (* Check and create any links that may not yet exist *)
    linkCheck (module CityAndCat) (data.city ^ data.category);
    linkCheck (module City) data.city;
    linkCheck (module Category) data.category;
  match hash with None -> failwith "no hash" | Some hash ->
  try
    CityLinks.commit
      (
        Links.t
      [|
        Links.Link.t
          ~tag:postsByUser
          ~base:me
          ~link:hash
          ();
        Links.Link.t
          ~base:(City.makeHash data.city)
          ~link:hash
          ~tag:postsByCity
          ();
        Links.Link.t
          ~base:(Category.makeHash data.category)
          ~link:hash
          ~tag:postsByCategory
          ();
        Links.Link.t
          ~base:cityAndCat
          ~link:hash
          ~tag:CityAndCat.name
          ()
      |]
      ) |> fun x -> Some x
  with e ->
    debug (sprintf "Error committing links %s" (Printexc.to_string e));
    None

(**
 * @param hash is hashedLink we are retrieving (ie. target value)
 * @param tag is the tag given when the link was created (ie. the relationship btwn link and base)
 * @returns an array of entries matching the hash given *)
let retrieveLinks
    (type entry)
    (module Entry : Entry.S with type t = entry) hash tag :
  entry array =
  (* if link doesn't exist then return emptyif (get(hash) === null) return [];*)
  (try
     Links.get ~tag ~base:hash
       ~options:
         (Links.Options.t
            ~load:true
            ~statusMask:Constants.System.Status.any
         )
   with e ->
     debug("Unable to retrieve links " ^ (Printexc.to_string e));
     [||]
  ) |>
  Links.unpack (module Entry) |> fun arr ->
  Belt_Array.map arr (fun {entry} -> entry)


(**
 * @returns all the posts of the current user
*)

let readYourPosts() =
  retrieveLinks (module PostData) App.Agent.hash postsByUser

(**
 * @param city name
 * @returns all the posts for the given city
 *)

let readPostsByCity city =
  retrieveLinks (module City) (City.makeHash city) postsByCity

(**
 * @param category name
 * @returns all the posts for the given category
 *)

let readPostsByCategory category =
  retrieveLinks (module Category) (Category.makeHash category) postsByCategory

(**
 * @param data is a JSON object: {"city":<name_of_city>, "category": <name_of_category}
 * @returns all the posts for the given city and category 
 *)

let readPostsByCityAndCategory (data:PostData.t) =
  let hashedCat = CityAndCat.makeHash
      (data.city ^ data.category) in
  retrieveLinks (module CityAndCat) hashedCat cityAndCategory

(**
 * @param data is a JSON object {"post":{<new data>}, "oldHash": "<previous_hash>"}
 * @returns hash of the updated post *)

let editPost post oldHash =
  try
    PostData.update post oldHash
  with e ->
    debug("Update not made: " ^ (Printexc.to_string e));
    oldHash

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
  let message = postHash ^ " deleted by " ^ App.Agent.hash in
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
  let me = App.Agent.hash in
  match PostData.get postHash with
  | None -> false
  | Some data ->
    let cityAndCat = CityAndCat.makeHash (data.city ^ data.category) in
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
                ~base:(City.makeHash data.city)
                ~link:postHash
                ~tag:postsByCity
                ~linkAction:System.LinkAction.del
                ()
              ;
              Link.t
                ~base:(Category.makeHash data.category)
                ~link:postHash
                ~tag:postsByCategory
                ~linkAction:System.LinkAction.del
                ()
              ;
              Link.t
                ~base:cityAndCat
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
  PostData.get hash ~options:(Entry.GetOptions.t ~local:true ())


include Builder.Build(Genesis.Success)(Sendreceive.Unit)

