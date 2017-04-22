module lab0

open System
open System.Net
open System.Collections.Specialized

let (email, name) = ("sharuev.ds@phystech.edu", "Шаруев Д. С.") // адрес почты и фамилия с инициалами

let pascal c r = 
  let rec C n k = 
    match (n, k) with
      | (n, k) when k = 0 || k = n -> 1
      | (n, k) when n < k -> 0
      | _ -> (C (n-1) k) + (C (n-1) (k-1))
  C (r) (c)

let printIt n = 
  "[" +
  ([for x in 0..n do for y in 0..x do yield pascal y x] 
    |> List.map (fun x -> x.ToString())
    |> List.reduce (fun x y -> x + "," + y) )
  + "]"

printIt 20

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("name", name)
  values.Add("content", printIt 20)

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab0"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

main ()