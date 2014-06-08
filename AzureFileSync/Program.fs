// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open System
open System.IO
open Microsoft.WindowsAzure.Storage
open System.Configuration
open System.Threading.Tasks;
open System.Linq


type EventType = File | Directory 
type FileSyncAction = Nothing | UpdateLocalFile
type FileShareToLocalDirMap = {ShareName : string; LocalFolder: string}
type AzureFileSyncOptions = {map: FileShareToLocalDirMap; fs: FileSystemWatcher}

let printFunc (eventType) (event : FileSystemEventArgs) = printfn "%s %s" eventType event.Name
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


let disableFsAndRun (f : unit -> unit) (path : string) = 
    ignoreFiles <- path :: ignoreFiles 
    f()
    let a = async {
            do! Async.Sleep(5000)
//            ignoreFiles <- 
            ignoreFiles <- ignoreFiles |> Seq.filter (fun x -> x = path) |> Seq.toList
        }
    Async.RunSynchronously(a) 

let rec mkdirLocal baseDir path = 
    match path with
    | [] -> ()
    | _ -> 
        let dirPath = System.IO.Path.Combine(baseDir,path.Head)
        match System.IO.Directory.Exists(dirPath) with   
        | false -> 
            let f = (fun ()-> System.IO.Directory.CreateDirectory(dirPath) |> ignore)
            disableFsAndRun f dirPath    
        | true -> ()
        mkdirLocal dirPath path.Tail

let getCloudItemShareRelativeUriParts (item : File.IListFileItem) =
    item.Uri.ToString().[item.Share.Uri.ToString().Length..].Split([|'/'|],StringSplitOptions.RemoveEmptyEntries) |> Seq.toList

let syncDirectory (cd : File.CloudFileDirectory) (map : AzureFileSyncOptions) =        
    let uriParts = cd |> getCloudItemShareRelativeUriParts 
    mkdirLocal map.map.LocalFolder uriParts

let getFileSyncAction path (cf : File.CloudFile) = 
    match path |> System.IO.File.Exists with
    | true -> 
        let fileInfo = new System.IO.FileInfo(path)        
        let cfTime = cf.Properties.LastModified.Value.ToUniversalTime()
        let locTime = new DateTimeOffset(fileInfo.LastWriteTimeUtc)
        printfn "cloudFile LastUpdated %s localFile LastUpdate %s" (cfTime.ToString(datetimeFormat)) (locTime.ToString(datetimeFormat))
        match cfTime > locTime with
        | true -> FileSyncAction.UpdateLocalFile
        | false -> FileSyncAction.Nothing
    | false -> FileSyncAction.UpdateLocalFile

    
let syncFile (cf : File.CloudFile) (map : AzureFileSyncOptions) = 
    let uriParts = cf |> getCloudItemShareRelativeUriParts 
    let allParts = [map.map.LocalFolder] @ uriParts |> Seq.toArray
    let path = System.IO.Path.Combine(allParts)
    match getFileSyncAction path cf with
    | FileSyncAction.Nothing -> ()
    | FileSyncAction.UpdateLocalFile -> 
        disableFsAndRun (fun () -> cf.DownloadToFile(path, FileMode.Create)) path

    
let rec syncFilesAndDirectories (dir : File.CloudFileDirectory) map = 
    dir.ListFilesAndDirectories() |> Seq.iter((fun file -> 
        match file with
        | :? File.CloudFile as cf -> 
            cf.FetchAttributes()
            printfn "cloud file %s %s"  cf.Name (cf.Properties.LastModified.Value.ToString(datetimeFormat))
            syncFile cf map
        | :? File.CloudFileDirectory as cd -> 
            cd.FetchAttributes()
            printfn "cloud directory %s %s" cd.Name (cd.Properties.LastModified.Value.ToString(datetimeFormat)) 
            syncDirectory cd map
            syncFilesAndDirectories cd map
        )) 

let syncLocalFilesAndDirecotryWithAzure map = 
    map.map.ShareName |> shareRoot |> (fun (dir) -> syncFilesAndDirectories dir map)

let syncLoop map = async {
    while true do
        syncLocalFilesAndDirecotryWithAzure map
        do! Async.Sleep 10000
}

let getEventType path : EventType = 
    match ((File.GetAttributes(path) &&& FileAttributes.Directory) = FileAttributes.Directory) with
    | true -> EventType.Directory
    | false -> EventType.File
    

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
     
     w.fs.Renamed.Add((fun e -> 
        printFunc "Renamed" e
     ))
     w.fs.Changed.Add((fun e -> 
        printFunc "Changed" e
        match (getEventType e.FullPath) with
        | EventType.Directory -> ()
        | EventType.File -> uploadFile e.FullPath w.map.ShareName
    ))
     w.fs.Created.Add((fun e ->
        printFunc "Created" e
        match (getEventType e.FullPath) with
        | EventType.Directory -> createDirectory e.FullPath w.map
        | EventType.File -> uploadFile e.FullPath w.map.ShareName
      ))
     w.fs.Deleted.Add((fun e -> 
        printFunc "Deleted" e
      ))
        ))

    directoryWatchers |> Seq.iter(( fun w ->
        let f = syncLoop w
        Async.RunSynchronously f
    ))
    
    Console.ReadLine()
    0 // return an integer exit code
