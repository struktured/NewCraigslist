let name = "postData"
type t =
  {
    title: string;
    category : string;
    subcategory: string Js.Null.t;
    city:string;
    email:string;
    timestamp:int;
  } [@@bs.deriving abstract]
