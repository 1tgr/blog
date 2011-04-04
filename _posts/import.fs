open System
open System.Globalization
open System.IO

type State = Yaml
           | AwaitingBodyPart
           | Body of string

type Format = Html
            | Markdown

type Post =
    {
        Title : string option
        Date : DateTime option
        Categories : Set<string>
        Tags : string option
        BaseName : string option
        Format : Format
        Body : string list
    }
    static member Empty : Post =
        {
            Title = None
            Date = None
            Categories = Set.empty
            Tags = None
            BaseName = None
            Format = Html
            Body = List.empty
        }

let (|Header|) (s : string) : string * string =
    match s.Split([| ':' |], 2) with
    | [| |] -> "", ""
    | [| left |] -> left.Trim(), ""
    | a -> a.[0].Trim(), a.[1].Trim()

[<EntryPoint>]
let Main _ =
    // Parse a text file in Movable Type format and write it back out as
    //  a set of text files in Blogofile format.
    try
        let rec impl (tr : TextReader) (posts : Post list) (state : State) : Post list =
            match tr.ReadLine() with
            | null -> posts

            | s ->
                let post, rest =
                    match posts with
                    | post :: rest -> post, rest
                    | [] -> Post.Empty, []

                let posts, state =
                    match state, s with
                    | Yaml, Header("TITLE", s) -> { post with Title = Some s } :: rest, Yaml
                    | Yaml, Header("BASENAME", s) -> { post with BaseName = Some s } :: rest, Yaml
                    | Yaml, Header("CONVERT BREAKS", s) when s.StartsWith("markdown") -> { post with Format = Markdown } :: rest, Yaml
                    | Yaml, Header("DATE", s) -> { post with Date = Some (DateTime.ParseExact(s, "MM/dd/yyyy hh:mm:ss tt", CultureInfo.InvariantCulture)) } :: rest, Yaml
                    | Yaml, Header(("CATEGORY" | "PRIMARY_CATEGORY"), s) -> { post with Categories = Set.add s post.Categories } :: rest, Yaml
                    | Yaml, Header("TAGS", s) -> { post with Tags = Some s } :: rest, Yaml
                    | (Yaml | Body _), "-----"  -> posts, AwaitingBodyPart
                    | AwaitingBodyPart, Header(part, "") -> posts, Body part
                    | Body "BODY", s -> { post with Body = s :: post.Body } :: rest, state
                    | _, "--------" -> Post.Empty :: posts, Yaml
                    | _ -> posts, state

                impl tr posts state

        let posts = 
            use sr = new StreamReader("/Users/Tim/Downloads/tim_robinson.txt")
            impl sr [] Yaml
            |> List.rev
            |> List.mapi (fun i post -> i, post)
        
        for i, post in posts do
            match post.Body, post.BaseName, post.Title with
            | _, None, None ->
                ()

            | l, _, _ when "" :: l |> List.forall (fun s -> s.Length = 0) ->
                ()

            | _, Some baseName, _
            | _, None, Some baseName ->
                let ext =
                    match post.Format with
                    | Html -> ".html"
                    | Markdown -> ".markdown"

                let filename = sprintf "%03d. %s%s" i baseName ext
                use sw = new StreamWriter(filename)
                
                let header name =
                    function
                    | Some (value : string) ->
                        let value =
                            if value.Contains(":") then
                                "\"" + value.Replace("\"", "\\\"") + "\""
                            else
                                value

                        fprintfn sw "%s: %s" name value

                    | None -> ()

                let date =
                    match post.Date with
                    | Some dt -> Some (dt.ToString("yyyy/MM/dd HH:mm:ss"))
                    | None -> None
                
                let concat set =
                    if Set.isEmpty set then
                        None
                    else
                        Some (String.concat "," set)

                fprintfn sw "---"
                header "title" post.Title
                header "date" date
                header "updated" date
                header "categories" (concat post.Categories)
                header "tags" post.Tags
                
                post.Date |> Option.iter (fun dt ->
                    let permalink = sprintf "/blog/%04d/%02d/%s.html" dt.Year dt.Month baseName
                    let permalink = permalink.Replace("_", "-")
                    header "permalink" (Some permalink))

                fprintfn sw "---"

                for line in List.rev post.Body do
                    fprintfn sw "%s" line

                printfn "Written %s" filename


        0
    with ex ->
        eprintfn "%O" ex
        1
