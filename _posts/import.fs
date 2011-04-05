open System
open System.Globalization
open System.IO
open System.Net
open System.Text.RegularExpressions

type State = Yaml
           | AwaitingBodyPart
           | Body of string
           | BodyCode of string

type Format = Html
            | Markdown

type Post =
    {
        Title : string option
        Date : DateTime option
        Categories : Set<string>
        Tags : string option
        BaseName : string
        Draft : bool
        Format : Format
        Body : string list
    }
    static member Empty : Post =
        {
            Title = None
            Date = None
            Categories = Set.empty
            Tags = None
            BaseName = ""
            Draft = false
            Format = Html
            Body = List.empty
        }

let (|Header|) (s : string) : string * string =
    match s.Split([| ':' |], 2) with
    | [| |] -> "", ""
    | [| left |] -> left.Trim(), ""
    | a -> a.[0].Trim(), a.[1].Trim()

let startRe = Regex("<pre( class=\"brush: (?<brush>[^\"]+)\")?>")
let endRe = Regex("</pre>")

let formatLine (line : string) : string =


    line

let parseLine (posts : Post list) (state : State) (s : String) : Post list * State =
    let post, rest =
        match posts with
        | post :: rest -> post, rest
        | [] -> Post.Empty, []

    match state, s with
    | Yaml, Header("TITLE", s) ->
        { post with Title = Some s } :: rest, Yaml

    | Yaml, Header("BASENAME", s) ->
        { post with BaseName = s } :: rest, Yaml

    | Yaml, Header("CONVERT BREAKS", s) when s.StartsWith("markdown") ->
        { post with Format = Markdown } :: rest, Yaml

    | Yaml, Header("DATE", s) ->
        { post with Date = Some (DateTime.ParseExact(s, "MM/dd/yyyy hh:mm:ss tt", CultureInfo.InvariantCulture)) } :: rest, Yaml

    | Yaml, Header(("CATEGORY" | "PRIMARY_CATEGORY"), s) ->
        { post with Categories = Set.add s post.Categories } :: rest, Yaml

    | Yaml, Header("TAGS", s) ->
        { post with Tags = Some s } :: rest, Yaml

    | Yaml, Header("STATUS", "Draft") ->
        { post with Draft = true } :: rest, Yaml

    | (Yaml | Body _), "-----"  ->
        posts, AwaitingBodyPart

    | AwaitingBodyPart, Header(part, "") ->
        posts, Body part

    | Body ("BODY" as part), s ->
        let line = startRe.Replace(s, fun (m : Match) ->
            let lang =
                match string (m.Groups.["brush"]) with
                | "plain" | "" -> "text"
                | "fsharp" -> "ocaml"
                | s -> s

            sprintf "$$code(lang=%s)" lang)

        let post = { post with Body = line :: post.Body }
        if line = s then
            post :: rest, state
        else
            post :: rest, BodyCode part

    | BodyCode ("BODY" as part), s ->
        let line = endRe.Replace(s, fun _ -> "$$/code")
        if line = s then
            { post with Body = WebUtility.HtmlDecode(s) :: post.Body } :: rest, state
        else
            { post with Body = line :: post.Body } :: rest, Body part

    | _, "--------" ->
        Post.Empty :: posts, Yaml

    | _ ->
        posts, state

let parse (tr : TextReader) : Post list =
    let rec impl posts state =
        match tr.ReadLine() with
        | null -> posts

        | s ->
            s
            |> parseLine posts state
            ||> impl

    [
        for post in List.rev (impl [] Yaml) do
            match post.Draft, post.Body with
            | true, _ -> ()
            | _, [] -> ()
            | _, l when l |> List.forall (fun s -> s.Length = 0) -> ()

            | _ ->
                yield { post with Body = List.rev post.Body }
    ]

[<EntryPoint>]
let Main _ =
    // Parse a text file in Movable Type format and write it back out as
    //  a set of text files in Blogofile format.
    try
        let posts = 
            using (new StreamReader("/Users/Tim/Downloads/tim_robinson.txt")) 
                  (parse >> List.mapi (fun i post -> i + 1, post))
        
        for i, post in posts do
            let ext =
                match post.Format with
                | Html -> ".html"
                | Markdown -> ".markdown"

            let filename = sprintf "%03d. %s%s" i post.BaseName ext
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
                let permalink = sprintf "/blog/%04d/%02d/%s.html" dt.Year dt.Month post.BaseName
                let permalink = permalink.Replace("_", "-")
                header "permalink" (Some permalink))

            fprintfn sw "---"

            for line in post.Body do
                fprintfn sw "%s" line

            printfn "Written %s" filename

        0
    with ex ->
        eprintfn "%O" ex
        1
