module Inigo.Async.Util

public export
toObject : String
toObject = "let toObject=((list)=>Object.fromEntries([...Array(list.h).keys()].map((i)=>{let x=list['a'+(i+1)];return [x.a1,x.a2]})));"

public export
toArray : String -> String
toArray str = "(function(){let toArray=(x)=>{if(x.h === 0){return []}else{return [x.a1].concat(toArray(x.a2))}};return " ++ str ++ "})()"

public export
promisifyPrim_ : String -> String
promisifyPrim_ str =
  "(...args)=>{let [w,err,ok,...fargs]=args.reverse();return (" ++ str ++ ")(...fargs.reverse()).then((x)=>ok(x)(w),(e)=>err(e)(w))}"

public export
promisifyPrim: String -> String
promisifyPrim str =
  "node:lambda:" ++ (promisifyPrim_ str)

public export
promisifyResolve: String -> String -> String
promisifyResolve res str =
  "node:lambda:" ++ promisifyPrim_ ( "(...inner)=>{(" ++ str ++ ")(...inner);return Promise.resolve(" ++ res ++ ");}" )
