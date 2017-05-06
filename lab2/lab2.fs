open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized
open System.Globalization
open System.Diagnostics
open System.Text.RegularExpressions

// почтовый адрес
let email = ""


type NumberType = Int of int|Float of float
type Token = OpenBrace|CloseBrace|OpenBracket|CloseBracket|Colon|Comma|Null|True|False|String of string |Number of NumberType
type JSON = Null|Boolean of bool|String of string|Number of NumberType|Array of JSON list|Object of (string*JSON) list

let explode (s:string) = [for i in s -> i]
let implode (xs:char list) =
  let sb = System.Text.StringBuilder(xs.Length)
  xs |> List.iter (sb.Append >> ignore)
  sb.ToString()

let tokenize (source:char list) : Token list = 
  let rec parseString (acc:String) (l:char list) : String*char list = 
    match l with
      | '"'::t -> (acc, t)
      | '\\'::'\"'::t -> parseString (acc+"\"") t
      | '\\'::'\\'::t -> parseString (acc+"\\") t
      | '\\'::'/'::t -> parseString (acc+"/") t
      | '\\'::'b'::t -> parseString (acc+"\b") t
      | '\\'::'f'::t -> parseString (acc+string(char 12)) t
      | '\\'::'n'::t -> parseString (acc+"\n") t
      | '\\'::'r'::t -> parseString (acc+"\r") t
      | '\\'::'t'::t -> parseString (acc+"\t") t
      | '\\'::'u'::a::b::c::d::t -> let s = implode ('\\'::'u'::a::b::c::[d])
                                    parseString (acc + Convert.ToString((char ( Int32.Parse(s.Substring(2), NumberStyles.HexNumber))))) t 
      | c::t -> parseString (acc+Convert.ToString(c)) t
      | _ -> failwith "string parse failed"

  let rec parseNumber (acc:String) (l:char list) : String*char list =  
    match l with
      | c::t when Char.IsDigit(c) || c = 'e' || c = 'E' || c = '-' || c = '+' || c = '.' -> parseNumber (acc+Convert.ToString(c)) t
      | t -> (acc, t)

  let rec tokenize' (acc:Token list) (str:char list) : Token list = 
    //Printf.printfn "%A" str
    match str with
      | [] -> acc
      | w::t when Char.IsWhiteSpace(w) -> tokenize' acc t
      | '{'::t -> tokenize' (OpenBrace::acc) t
      | '}'::t -> tokenize' (CloseBrace::acc) t
      | '['::t -> tokenize' (OpenBracket::acc) t
      | ']'::t -> tokenize' (CloseBracket::acc) t
      | ':'::t -> tokenize' (Colon::acc) t
      | ','::t -> tokenize' (Comma::acc) t
      | 'n'::'u'::'l'::'l'::t -> tokenize' (Token.Null::acc) t
      | 't'::'r'::'u'::'e'::t -> tokenize' (True::acc) t
      | 'f'::'a'::'l'::'s'::'e'::t -> tokenize' (False::acc) t
      | '"'::t -> let s, t' = parseString "" t 
                  tokenize' ((Token.String(s))::acc) t'
      | c::t when Char.IsDigit(c) || c = '-' -> let s, t' = parseNumber "" (c::t)
                                                if not (Regex("\A[\-]?([0]|[1-9][0-9]*)([\.][0-9]+)?([eE][\+\-]?[0-9]+)?\z")
                                                  .IsMatch(s)) then failwith "Malformed number"
                                                else 
                                                  if s.Contains(".") || s.Contains("E") || s.Contains("e") then
                                                    tokenize' ((Token.Number(NumberType.Float(float s)))::acc) t'
                                                  else tokenize' ((Token.Number(NumberType.Int(int s)))::acc) t'
      | _ -> Printf.printfn "%A\n%A" str acc
             failwith "tokenize failed"

  List.rev (tokenize' [] source)

let parse source = 
  let rec parse' json = 
    // obj - аккумулятор для текущего объекта
    let rec parseObject (obj:(string*JSON) list) json = 
      //Printf.printfn "%A" json
      match json with
      | Token.String name::Token.Colon::ts -> let (parsed, ts) = parse' ts 
                                              parseObject ((name, parsed)::obj) ts
      | Token.Comma::ts -> parseObject obj ts
      | Token.CloseBrace::ts -> (JSON.Object (List.rev obj), ts)
      | _ -> failwith "Fail in parseObject"
    
    let rec parseArray (arr:JSON list) = function
      | Token.Comma::t -> parseArray arr t
      | Token.CloseBracket::t -> (JSON.Array (List.rev arr), t)
      | t -> let (parsed, t) = parse' t
             parseArray (parsed::arr) t
      
    //Printf.printfn "%A" json
    match json with 
      | Token.Null::t -> JSON.Null, t
      | Token.OpenBrace::t -> parseObject [] t
      | Token.OpenBracket::t -> parseArray [] t
      | Token.String s::t -> (JSON.String s, t)
      | Token.Number n::t -> (JSON.Number n, t)
      | Token.True::t -> (JSON.Boolean true, t)
      | Token.False::t -> (JSON.Boolean false, t)
      | _ -> failwith "Invalid JSON object start"


  match parse' source with
    | res, [] -> res
    | _, _ -> failwith "Malformed token list"

let tree (s:String) = s |> explode |> tokenize |> parse

let example1 = """{"a":1, "b":2}"""
let res1 = tree example1
let example2 = 
  """
  {
    "a":"str",
    "b":10,
    "c":[-1e4, 1e+5, 3.14, 1.4E-1],
    "d":true,
    "e":{"a":1, "b":2},
    "f":null,
    "sun":"\u2600",
    "escapes":["\"", "\\", "\/", "\n", "\r", "\t"]
  } 
  """
let res2 = tree example2

let example3 = 
  """
  {
      "glossary": {
          "title": "example glossary",
      "GlossDiv": {
              "title": "\u0482",
        "GlossList": {
                  "GlossEntry": {
                      "ID": "SGML",
            "SortAs": "SGML",
            "GlossTerm": "Standard Generalized Markup Language",
            "Acronym": -1.2e-6,
            "Abbrev": 42,
            "GlossDef": {
                          "para": "A meta-markup language, used to create markup languages such as DocBook.",
              "GlossSeeAlso": ["GML", "XML"]
                      },
            "GlossSee": "markup"
                  }
              }
          }
      }
  }
  """
let res3 = tree example3

let rec lab3 (node:JSON) : JSON =
  match node with
    | JSON.Object o -> 
      JSON.Object <| List.map (fun ((key, value) : string*JSON) -> 
        match value with
          | JSON.String value -> (value, JSON.String(key)) 
          | _ -> (string(key), lab3 value)
      ) o
    | JSON.Array a ->
      JSON.Array <| List.map (lab3) a
    | o -> o

lab3(tree(example1))
lab3(tree(example2))
lab3(tree(example3))

let example4 =
  """
  {
    "a":{"l":"r", "l":"r"},
    "b":["a", "b", "c"],
    "c":"d"
  }
  """
lab3(tree(example4))
let stringify = function
  | Object list -> "{}"

let generate = 
  let rnd = new Random()
  match rnd.Next(42) with
    | 0 -> Object []
    | _ -> Object [("random", Object [])]

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
