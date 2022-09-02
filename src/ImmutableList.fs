module System.Collections.Immutable.FSharp.ImmutableList

open System
open System.Collections.Immutable

module Patterns =
  
  [<return: Struct>]
  let (|Empty|_|) (l:ImmutableList<'a>) =
    match l.Count with
    | 0 -> ValueSome Empty
    | _ -> ValueNone

  [<return: Struct>]
  let (|TakeElem|_|) i (l:ImmutableList<'a>) =
    match i-1 > l.Count with
    | true -> ValueNone
    | false -> 
      let li = l.[i]
      let l = l.RemoveAt i
      ValueSome (struct(li,l))

  [<return: Struct>]
  let (|TakeFirst|_|) l =
    match l with
    | TakeElem 0 lp -> ValueSome lp
    | _ -> ValueNone

  [<return: Struct>]
  let (|TakeLast|_|) (l:ImmutableList<'a>) =
    let i = l.Count - 1
    match l with
    | TakeElem i (v,rem) -> ValueSome (struct(rem,v))
    | _ -> ValueNone

  [<return: Struct>]
  let rec (|TakeFirstN|_|) n (l:ImmutableList<'a>) =
    match n with
    | _ when n < 0 -> raise (ArgumentOutOfRangeException "n must not be negative")
    | 0 -> ValueSome (struct([],l))
    | _ ->
      let n = n-1
      match l with
      | TakeFirst (e,l) -> 
        match (|TakeFirstN|_|) (n-1) l with
        | ValueNone -> ValueNone
        | ValueSome (es,l) -> ValueSome (struct(e::es,l))
      | _ -> ValueNone