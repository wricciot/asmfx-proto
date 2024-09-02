let (|>) x f = f x
let (<|) f x = f x

let unopt default = function None -> default | Some x -> x
