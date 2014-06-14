open System

type EventAction = Nothing | Something
type TimeEvent = {time: DateTime; Id: int; Action : EventAction}

let t,t2 =  new System.Timers.Timer(1000.0), new System.Timers.Timer(5000.0)


let timer1Stream  = t.Elapsed |> Observable.map(fun _ -> {time = DateTime.Now; Id = 1; Action = EventAction.Something})
let timer2Stream  = t2.Elapsed |> Observable.map(fun _ -> {time = DateTime.Now; Id = 1; Action = EventAction.Nothing})


let mergedStream = Observable.merge timer1Stream timer2Stream 

// If an EventAction.Nothing flows through the all subsequent Events with the same Id as the EventAction.Nothing will be filtered out, until the next EventAction.Nothing
let filter l e = 
    printfn "filter func says: %A List contains:  %A" e l
    let res = l |> Seq.filter(fun i -> i.Action = EventAction.Nothing) |> Seq.toList
    match e.Action with
    | EventAction.Nothing -> 
        match res |> Seq.exists(fun i -> i.Id = e.Id) with
        | true -> res|>Seq.filter(fun i -> i.Id <> e.Id) |> Seq.toList
        | false -> e::res
    | EventAction.Something -> 
        match res |> Seq.exists(fun i -> i.Id = e.Id) with
        | true -> res
        | false -> e::res

let result = 
    mergedStream 
    |> Observable.scan(filter) [] 
    |> Observable.map(fun e -> e |> Seq.filter(fun i -> i.Action= EventAction.Something)) 
    |> Observable.subscribe(fun e -> printfn "result: %A" e)

t.Start()
t2.Start()

t.Stop()
t2.Stop()
