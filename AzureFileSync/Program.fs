// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open System
open System.IO
open Microsoft.WindowsAzure.Storage
open System.Configuration
open System.Threading.Tasks;
open System.Linq

type SynchronizingEvent = {Filename: string}
type ItemType = File | Directory 
type EventType = Changed | Renamed | Added | Deleted | Ignore
type FileSyncAction = Nothing | UpdateLocalFile
type FileShareToLocalDirMap = {ShareName : string; LocalFolder: string}
type AzureFileSyncOptions = {map: FileShareToLocalDirMap; fs: FileSystemWatcher}

type FileEvent = {event: SynchronizingEvent; eventType : EventType; time: DateTime}

let printFunc (event : FileEvent) = printfn "%s %A" event.event.Filename event.eventType
let datetimeFormat = "dd.MM.yyyy HH:mm:ss"   


let createCloudStorageAccount = 
    CloudStorageAccount.Parse(ConfigurationManager.AppSettings.Get("connectionString"))
let createCloudFileClient = 
    createCloudStorageAccount.CreateCloudFileClient()
let createCloudShare name = 
    let client = createCloudFileClient
    let shareReference = client.GetShareReference(name)
    shareReference.CreateIfNotExistsAsync().Wait()
    shareReference

let shareRoot name = 
    let share = createCloudShare name
    share.GetRootDirectoryReference()

let mutable ignoreFiles = []

let uploadFile filePath shareName =        
    if (ignoreFiles |> Seq.exists(fun s -> s = filePath)) then
        ()
    else
        let fileReference = (shareRoot shareName).GetFileReference(System.IO.Path.GetFileName(filePath))
        use f = File.Open(filePath,FileMode.Open)    
        fileReference.UploadFromStreamAsync(f).Wait() 


let rec mkdir (dirReference : File.CloudFileDirectory) path = 
    match path with
    | [] -> ()
    | _ -> 
         let dir = dirReference.GetDirectoryReference path.Head
         dir.CreateIfNotExists() |> ignore
         mkdir dir path.Tail



let createDirectory (filePath : string) (map : FileShareToLocalDirMap) = 
    let pathParts = filePath.[map.LocalFolder.Length..] //substring
    let paths = pathParts.Split([|System.IO.Path.DirectorySeparatorChar|],StringSplitOptions.RemoveEmptyEntries) |> Seq.toList

    let dirReference = (shareRoot map.ShareName)
    mkdir dirReference paths


//let disableFsAndRun (f : unit -> unit) (path : string) = 
//    ignoreFiles <- path :: ignoreFiles 
//    f()
//    let a = async {
//            do! Async.Sleep(5000)
////            ignoreFiles <- 
//            ignoreFiles <- ignoreFiles |> Seq.filter (fun x -> x = path) |> Seq.toList
//        }
//    Async.RunSynchronously(a) 

let rec mkdirLocal baseDir path (event : Event<SynchronizingEvent>) = 
    match path with
    | [] -> ()
    | _ -> 
        let dirPath = System.IO.Path.Combine(baseDir,path.Head)
        match System.IO.Directory.Exists(dirPath) with   
        | false -> 
            event.Trigger({Filename = dirPath})
            System.IO.Directory.CreateDirectory(dirPath) |> ignore
            event.Trigger({Filename = dirPath})
        | true -> ()
        mkdirLocal dirPath path.Tail event

let getCloudItemShareRelativeUriParts (item : File.IListFileItem) =
    item.Uri.ToString().[item.Share.Uri.ToString().Length..].Split([|'/'|],StringSplitOptions.RemoveEmptyEntries) |> Seq.toList

let syncDirectory (cd : File.CloudFileDirectory) (map : AzureFileSyncOptions) event =        
    let uriParts = cd |> getCloudItemShareRelativeUriParts 
    mkdirLocal map.map.LocalFolder uriParts event

let getFileSyncAction path (cf : File.CloudFile) = 
    match path |> System.IO.File.Exists with
    | true -> 
        let fileInfo = new System.IO.FileInfo(path)        
        let cfTime = cf.Properties.LastModified.Value.ToUniversalTime()
        let locTime = new DateTimeOffset(fileInfo.LastWriteTimeUtc)
//        printfn "cloudFile LastUpdated %s localFile LastUpdate %s" (cfTime.ToString(datetimeFormat)) (locTime.ToString(datetimeFormat))
        match cfTime > locTime with
        | true -> FileSyncAction.UpdateLocalFile
        | false -> FileSyncAction.Nothing
    | false -> FileSyncAction.UpdateLocalFile

    
let syncFile (cf : File.CloudFile) (map : AzureFileSyncOptions) (event : Event<SynchronizingEvent>) = 
    let uriParts = cf |> getCloudItemShareRelativeUriParts 
    let allParts = [map.map.LocalFolder] @ uriParts |> Seq.toArray
    let path = System.IO.Path.Combine(allParts)
    match getFileSyncAction path cf with
    | FileSyncAction.Nothing -> ()
    | FileSyncAction.UpdateLocalFile -> 
        event.Trigger({Filename = path})
        cf.DownloadToFile(path, FileMode.Create)
        event.Trigger({Filename = path})
//        disableFsAndRun (fun () -> cf.DownloadToFile(path, FileMode.Create)) path

    
let rec syncFilesAndDirectories (dir : File.CloudFileDirectory) map event = 
    dir.ListFilesAndDirectories() |> Seq.iter((fun file -> 
        match file with
        | :? File.CloudFile as cf -> 
            cf.FetchAttributes()
//            printfn "cloud file %s %s"  cf.Name (cf.Properties.LastModified.Value.ToString(datetimeFormat))
            syncFile cf map event
        | :? File.CloudFileDirectory as cd -> 
            cd.FetchAttributes()
//            printfn "cloud directory %s %s" cd.Name (cd.Properties.LastModified.Value.ToString(datetimeFormat)) 
            syncDirectory cd map event
            syncFilesAndDirectories cd map event
        )) 

let syncLocalFilesAndDirecotryWithAzure map event = 
    map.map.ShareName |> shareRoot |> (fun (dir) -> syncFilesAndDirectories dir map event)

let syncLoop map event = async {
    
    while true do
        printfn "sync in progress"
        syncLocalFilesAndDirecotryWithAzure map event
        printfn "sync in done"
        do! Async.Sleep 10000   
}

let getItemType path : ItemType = 
    match ((File.GetAttributes(path) &&& FileAttributes.Directory) = FileAttributes.Directory) with
    | true -> ItemType.Directory
    | false -> ItemType.File
    
let filter l e = 
    //printfn "filter func says: %A List contains:  %A" e l
    let res = l |> Seq.filter(fun i -> i.eventType = EventType.Ignore) |> Seq.toList
    match e.eventType with
    | EventType.Ignore-> 
        match res |> Seq.exists(fun i -> i.event.Filename = e.event.Filename) with
        | true -> res|>Seq.filter(fun i -> i.event.Filename <> e.event.Filename) |> Seq.toList
        | false -> e::res
    | _ -> 
        match res |> Seq.exists(fun i -> i.event.Filename = e.event.Filename) with
        | true -> res
        | false -> e::res

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let dirs = [| {ShareName ="myshare"; LocalFolder = "c:\\temp"}|]
    let directoryWatchers = dirs |> Seq.map((fun d -> 
        printfn "%A" d
        {map=d; fs=new FileSystemWatcher(d.LocalFolder, EnableRaisingEvents = true, IncludeSubdirectories = true)}
        ))    


    directoryWatchers
     |> Seq.iter((fun w -> 
         let event = new Event<SynchronizingEvent>()
         let syncTriggeredEvents = event.Publish |> Observable.map(fun e -> {event = e; eventType = EventType.Ignore; time = DateTime.Now})
         let f = syncLoop w event
         
            
         let renamedEventStream = w.fs.Renamed |> Observable.map(fun(e) -> {event = {Filename = e.FullPath}; eventType = EventType.Renamed; time = DateTime.Now})
         let changedEventStream = w.fs.Changed |> Observable.map(fun(e) -> {event = {Filename = e.FullPath}; eventType = EventType.Changed; time = DateTime.Now})
         let createdEventStream = w.fs.Created |> Observable.map(fun(e) -> {event = {Filename = e.FullPath}; eventType = EventType.Added; time = DateTime.Now})
         let deletedEventStream = w.fs.Deleted |> Observable.map(fun(e) -> {event = {Filename = e.FullPath}; eventType = EventType.Deleted; time = DateTime.Now})

         let combinedStream = 
            syncTriggeredEvents 
            |> Observable.merge renamedEventStream
            |> Observable.merge changedEventStream 
            |> Observable.merge createdEventStream
            |> Observable.merge deletedEventStream
         
         combinedStream |> Observable.scan(filter) [] |> Observable.subscribe(fun l -> 
            l |> Seq.iter (fun e -> (
                printFunc e
                match e.eventType with 
                | EventType.Added ->
                    match (getItemType e.event.Filename) with
                    | ItemType.Directory -> createDirectory e.event.Filename w.map
                    | ItemType.File -> uploadFile e.event.Filename w.map.ShareName
                | EventType.Changed ->             
                    match (getItemType e.event.Filename) with
                    | ItemType.Directory -> ()
                    | ItemType.File -> uploadFile e.event.Filename w.map.ShareName |> ignore
                | EventType.Deleted -> ()
                | EventType.Renamed -> ()
                | EventType.Ignore -> ()
            ))
         ) |> ignore
      
         Async.RunSynchronously f
    ))

    
    Console.ReadLine()
    0 // return an integer exit code
