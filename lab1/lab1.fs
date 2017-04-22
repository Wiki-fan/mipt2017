module lab1

open System
open System.Net
open System.IO
open System.Collections.Specialized
open Microsoft.FSharp.Math

// почтовый адрес
let email = "sharuev.ds@phystech.edu"
// общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = float * int
let delta = 1e-10

// *** Первая часть

let fTailor x : float = (1./4.)*(x**2. - Math.PI**2./3.) // функция, которую раскладываем
let n, a, b = 20., Math.PI/5., Math.PI // интервал

// Простой способ
let tailor (x:float) : Result =
    let fValue = fTailor x

    let getNthPart (n:int) =
        match n with
            | 0 -> 0.
            | _ -> (if n%2 = 0 then 1. else -1.)*Math.Cos((float n)*x)/((float n)**2.)

    let rec tailor' acc n = 
        if (Math.Abs(acc-fValue) <delta) then
            (acc, n)
        else 
            let curPart = getNthPart n
            tailor' (acc+curPart) (n+1)
                    
    tailor' 0. 0

// Способ с аккумулятором
let tailorA x : Result =
    let fValue = fTailor x

    let sinx = Math.Sin(x)
    let cosx = Math.Cos(x)

    let getTrigNplus1 (sinnx, cosnx) = 
        let cosnplus1 = cosx*cosnx-sinx*sinnx
        let sinnplus1 = sinx*cosnx+cosx*sinnx
        (sinnplus1, cosnplus1)

    let getPart n trigN = 
        match n with
            | 0 -> 0.
            | _ -> ((-1.**(float n))*(snd trigN) / ((float n)** 2.))

    let rec tailor' (acc:float) trigN (n:int) : Result = 
        if (Math.Abs(acc-fValue) < delta) then
            //printf "%f %f Gotcha\n" acc fValue
            //printf "%.15f %.15f %.15f %b %.15f Gotcha\n" acc fValue (Math.Abs(acc-fValue)) ((Math.Abs(acc-fValue)) < delta) delta
            (acc, n)
        else
            let trigNplus1 = getTrigNplus1 trigN
            let curPart = getPart (n) trigN
            //printf "%f %f\n" curPart (getNthPart n)
            tailor' (acc+curPart) trigNplus1 (n+1)
                
    tailor' 0. (Math.Sin(0.), Math.Cos(0.)) 0

let printTailor () = 
    printf "%-15s %-15s %-7s %-15s %-7s %-15s\n" "x" "Naive way" "Iters" "Accumulated way" "Iters" "Function value"
    // b-delta, потому что на моей машине вычисление косинуса в окрестности PI*n даёт очень большую ошибку.
    [a .. (b-a)/n .. b-delta] 
    |> List.map (fun x -> 
        let (firstRes, firstCou), (secondRes, secondCou) = tailor x, tailorA x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
    |> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )

//printTailor()

// *** Вторая часть

// функция, решение которой ищем
let functionsToSolve = [
    fun x -> Math.Exp(x)-Math.Exp(-x)-2.;
    fun x -> Math.Sin(Math.Log(x))-Math.Cos(Math.Log(x))+2.*Math.Log(x);
    fun x -> x-2.+Math.Sin(1./x)
]
let intervals = [(0., 1.); (1., 3.); (1.2, 2.)]
let correctResults = [0.8814; 1.3749; 1.3077]

let internal f' = fun f x -> (f(x+delta)-f(x))/delta

let iter f a b : Result = 
    let x0 = (a+b)/2.
    let lambda = 0.01*(float (Math.Sign (f' f x0)))
    let rec iter' n x : Result =
        let next = x - lambda*f(x)
        match next with
            | next when Math.Abs(f x) < delta -> (next, n)
            | _ -> iter' (n+1) next
    iter' 0 x0

let newton f a b : Result =
    let rec newton' n x : Result =
        let next = x - (f x)/(f' f x)
        match next with
            | next when Math.Abs(f x) < delta -> (next, n)
            | _ -> newton' (n+1) next
    newton' 0 ((a+b)/2.)

let dichotomy (f:float->float) =
    // для функций с аккумулятором удобно ставить его в начало
    let rec dichotomyA i (a:float) (b:float) : Result = 
        let x = (a+b)/2.
        match x with
            | x when Math.Abs(f x) < delta -> ((a+b)/2., i)
            | x when (f a)*(f x) < 0. -> dichotomyA (i+1) a x
            | _ -> dichotomyA (i+1) x b
        
    dichotomyA 0 // чтобы воспользоваться каррированием

let printSolve fSolve (a, b) =
    printf "%13s\t%13s\t%13s\n" "iter" "newton" "dichotomy"
    [iter; newton; dichotomy] 
    |> List.map (fun f -> f fSolve a b) 
    |> List.iter (fun (res, cou) -> printf "%8f %4d\t" res cou)
    printf "\n"

let printAll() =
    List.iter2 printSolve functionsToSolve intervals

//printAll ()

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab1"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

main()
