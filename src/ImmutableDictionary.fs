module System.Collections.Immutable.FSharp.ImmutableDictionary

open System.Collections.Generic
open System.Collections.Immutable

let ofStructTupleSeq s =
  ImmutableDictionary.ToImmutableDictionary
    (
      s,
      (fun struct(k,_) -> k),
      (fun struct(_,v) -> v)
    )

let ofTupleSeq s =
  ImmutableDictionary.ToImmutableDictionary (s, fst, snd)

let ofKeyValuePairSeq (s: KeyValuePair<'k,'v> seq) =
  ImmutableDictionary.ToImmutableDictionary s

let add key value (dict:ImmutableDictionary<'k,'v>) = dict.Add(key,value)

let setItem key value (dict:ImmutableDictionary<'k,'v>) = dict.SetItem(key,value)

let count (dict:ImmutableDictionary<'k,'v>) = dict.Count

let keys (dict:ImmutableDictionary<'k,'v>) = dict.Keys

let values (dict:ImmutableDictionary<'k,'v>) = dict.Values


module Patterns =
  
  [<return: Struct>]
  let (|Empty|_|) (d:ImmutableDictionary<'k,'v>) =
    match d.Count with
    | 0 -> ValueSome Empty
    | _ -> ValueNone

  [<return: Struct>]
  let (|TakeElem|_|) k (d:ImmutableDictionary<'k,'v>) =
    match d.TryGetValue k with
    | (true,v) -> 
      ValueSome (struct(v,d.Remove k))
    | _ -> ValueNone