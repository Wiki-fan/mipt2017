open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized
open System.Globalization
open System.Diagnostics
open System.Text.RegularExpressions

// почтовый адрес
let email = "sharuev.ds@phystech.edu"


type NumberType = Int of int|Float of float
type Token = OpenBrace|CloseBrace|OpenBracket|CloseBracket|Colon|Comma|Null|True|False|String of string |Number of NumberType
type JSON = Null|Boolean of bool|String of string|Number of NumberType|Array of JSON list|Object of (string*JSON) list

let explode (s:string) = [for i in s -> i]
let implode (cl:char list) = List.fold (fun acc c -> acc+string(c)) "" cl

let tokenize (source:char list) : Token list = 
  let rec parseString (acc:String) (l:char list) : String*char list = 
    match l with
      | '"'::t -> (acc, t)
      | '\\'::'\"'::t -> parseString (acc+"\"") t
      | '\\'::'\\'::t -> parseString (acc+"\\") t
      | '\\'::'/'::t -> parseString (acc+"/") t
      | '\\'::'b'::t -> parseString (acc+"\b") t
      | '\\'::'f'::t -> parseString (acc+"\u000C") t
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
let example4 =
  """
  {
    "a":{"l":"r", "l":"r"},
    "b":["a", "b", "c"],
    "c":"d"
  }
  """

let res1 = tree example1
let res2 = tree example2
let res3 = tree example3
let res4 = tree example4

// Ключ может быть только строкой, а значение — чем угодно. Получается, сделать требуемое нужно только для пар вида str1:str2.
// (вариант, в котором мы рекурсивно заходим в поддеревья, обрабатываем их, а потом при помощи stringify делаем из значения строку и таки меняем
// ключ и значение местами, очень странно выглядит)
let rec lab2 (node:JSON) : JSON =
  match node with
    | JSON.Object o -> 
      JSON.Object <| List.map (fun ((key, value) : string*JSON) -> 
        match value with
          | JSON.String value -> (value, JSON.String(key)) 
          | _ -> (string(key), lab2 value)
      ) o
    | JSON.Array a ->
      JSON.Array <| List.map (lab2) a
    | o -> o

lab2(tree(example1))
lab2(tree(example2))
lab2(tree(example3))
lab2(tree(example4))

let rec stringify (json:JSON):string = 
  let tab = "\t"
  let escape (s:string):string = 
    let escape' (c:char) : char list = 
      match c with
      | '\"' -> ['\\';'\"']
      | '\\' -> ['\\';'\\']
      | '/' -> ['\\';'/']
      | '\b' -> ['\\';'b']
      | '\u000C' -> ['\\';'f']
      | '\n' -> ['\\';'n']
      | '\r' -> ['\\';'r']
      | '\t' -> ['\\';'t']
      | c -> [c]
    
    implode(List.concat (List.map escape' (explode s)))

  let wrap s = 
    "\""+(escape s)+"\""
  match json with
  | JSON.Null -> "null"
  | JSON.Boolean b -> if b then "true" else "false"
  | JSON.Number(NumberType.Float f)-> string(f)
  | JSON.Number(NumberType.Int n) -> string(n)
  | JSON.String s -> wrap s
  | JSON.Array a -> "["+(String.concat ", " (List.map stringify a))+"]"
  | JSON.Object o -> "{"+(String.concat ", " (List.map (fun field -> (wrap (fst field)) + " : " + (stringify (snd field))) o))+"}" 

stringify res1
stringify res2
stringify res3
stringify res4

let generate () : JSON = 
  let MAX_STRING_LENGTH = 10
  let MAX_KEY_LENGTH = 10
  let MAX_OBJECT_LEN = 10
  let MAX_ARRAY_LEN = 10
  let MAX_DEPTH = 10
  let rnd = new Random()

  let getRandChar() = char((int 'a')+(rnd.Next(int 'z' - int 'a')))
  let getRandString length : string = List.fold (fun acc c -> acc+(string c)) "" ([for i in 0..length -> getRandChar()])

  let rec generate' (depth:int) = 
    if depth=0 then JSON.Null else 
      match rnd.Next 7 with 
        | 0 -> JSON.Null
        | 1 -> JSON.Boolean ((rnd.Next 2) = 0)
        | 2 -> JSON.Number(NumberType.Int(rnd.Next()))
        | 3 -> JSON.Number(NumberType.Float(rnd.NextDouble()))
        | 4 -> JSON.String(getRandString (rnd.Next MAX_STRING_LENGTH))
        | 5 -> JSON.Object([for i in 0..(rnd.Next MAX_OBJECT_LEN) -> (getRandString (rnd.Next MAX_KEY_LENGTH), generate' (depth-1)) ])
        | 6 -> JSON.Array([for i in 0..(rnd.Next MAX_ARRAY_LEN) -> generate' (depth-1)])
        | _ -> failwith "Impossible match"
  generate' MAX_DEPTH

let randtree = generate()
printf "%s" (String.Concat [(stringify (randtree)); "\n"; (stringify (lab2(randtree)))])
(stringify (randtree)) = (stringify (lab2(lab2(randtree))))

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

main()
