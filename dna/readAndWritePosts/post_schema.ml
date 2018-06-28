type t =
  {
    title: string;
    category : string;
    subcategory: string option;
    city:string;
    email:string;
    timestamp:int;
  } [@@deriving bs.abstract]
