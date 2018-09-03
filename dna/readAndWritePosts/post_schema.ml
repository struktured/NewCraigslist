let name = "postData"
type t =
  {
    title: string;
    category : string;
    subcategory: string [@bs.optional];
    details: string;
    city:string;
    email:string;
    timestamp:int;
  } [@@bs.deriving abstract]
