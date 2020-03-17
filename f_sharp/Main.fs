// Extension to the Array type to allow equality comparison (via Array.Equals) of arrays
// Order-insensitive: checks only that all elements are common. 
open System

module Array =

   let rec EqualsHelper (arr1: array<int>) (arr2: array<int>) = 
      match arr1 with 
      | [||] -> true 
      | _ ->  
         let head = arr1.[0]
         let tail = arr1.[1..]
         if Array.contains head arr2 then 
            EqualsHelper tail arr2 
         else 
            false

   let Equals arr1 arr2 = 
      if Array.length arr1 = Array.length arr2 then 
         EqualsHelper arr1 arr2 
      else 
         false 
         
let RowCol2index row col = 
   ((row-1) * 9) + (col) - 1

let index2RowCol ind = 
   ((ind / 9) + 1 , (ind % 9) + 1)

// Could store potentials for each position within the matrix as a list associated with that posn. 
// Or, to reduce memory usage, simply recalculate this value each time on demand... much slower!
// Matrix type definition --------------------------------------------------
type Matrix(rows: array<int>) = 
   let mutable rows = rows
   member this.Row index = 
      let low = ((index-1) * 9)
      let high = (index * 9) - 1
      rows.[low..high]

   member this.Column col = 
      [| for i in 1..9 -> rows.[ RowCol2index i col ] |]

   member this.Set x atRow atCol = 
      rows.[ RowCol2index atRow atCol ] <- x
     
   member this.Get atRow atCol = 
      rows.[ RowCol2index atRow atCol ]

   member this.WhichThird row = 
      let list = [1;2;3]
      if row <= 3 then [ for l in list -> 1 * l ]
      elif row <= 6 then [ for l in list -> 2 * l ] 
      else [ for l in list -> 3 * l ]

   member this.Box row col = 
      let rowList = this.WhichThird row 
      let colList = this.WhichThird col 
      [| for row in rowList do
         for col in colList do 
            yield this.Get row col |]

   member this.Print = 
      for i in 1..9 do 
         let a = Array.map (fun i -> if i <> 0 then string(i) + " " else "  ") (this.Row(i))
         printfn "%A" (System.String.Concat(a))
     
   member this.AsArray =
      rows 

// Soduku type definition --------------------------------------------------
type Soduku(rows: array<int>) = 
   let mutable matrix = Matrix(rows) 
   let mutable potentials = [||]

   member this.CalculatePotentials (matrix: Matrix) = 
      let rawPs = [| for r in Soduku.fullSet do
                     for c in Soduku.fullSet -> 
                        this.MissingNumbers r c |] 
      Array.fold2 (fun acc elem potens -> 
         if elem <> 0 then 
            Array.append acc [| [||] |]
         else 
            Array.append acc [| potens |] ) [||] matrix.AsArray rawPs               
         
   static member fullSet = [|1..9|]

   member this.RowComplete index =
      matrix.Row(index) = Soduku.fullSet

   member this.ColumnComplete index = 
      matrix.Column(index) = Soduku.fullSet

   member this.BoxComplete row col = 
      (matrix.Box row col) = Soduku.fullSet

   member this.Print = 
      matrix.Print

   static member FilterZero arr = 
      Array.filter (fun x -> x <> 0) arr

   member this.MissingNumbers forRow forColumn = 
      let boxSet = Soduku.FilterZero (matrix.Box forRow forColumn)
      let rowSet = Soduku.FilterZero (matrix.Row forRow)
      let colSet = Soduku.FilterZero (matrix.Column forColumn) 
      let union = (set boxSet) + (set rowSet) + (set colSet)
      Set.toArray ((set Soduku.fullSet) - union)
   
   member this.NextDeterminateElement =
      let ind = Array.findIndex (fun pList -> Array.length pList = 1) potentials
      let RC = index2RowCol ind   
      printfn "Next determinate element at index %A, position %A" ind RC 
      RC 

   member this.GetPotentialNumber ind = 
      Array.head (potentials.[ind])

   member this.Solve = 
      potentials <- this.CalculatePotentials matrix
      let nextDet = this.NextDeterminateElement
      let ind = RowCol2index (fst nextDet) (snd nextDet)
      let nextNum = this.GetPotentialNumber ind
      printfn "Missing number is %A" nextNum
      matrix.Set nextNum (fst nextDet) (snd nextDet)
      this.Print

   member this.Complete = 
      Array.TrueForAll (potentials, ( fun x -> Array.isEmpty x ))
      

// Script starts here ---------------------------------------------------------      

[<EntryPoint>]
let main argv = 

   let rows = 
      [| 0; 0; 5; 6; 1; 0; 0; 9; 4;
         7; 0; 0; 3; 0; 5; 0; 0; 0; 
         0; 0; 0; 0; 9; 0; 0; 5; 7;
         8; 1; 0; 4; 0; 0; 5; 0; 0;
         6; 0; 7; 9; 0; 1; 8; 0; 2; 
         0; 0; 2; 0; 0; 6; 0; 7; 9; 
         9; 6; 0; 0; 5; 0; 0; 0; 0; 
         0; 0; 0; 2; 0; 3; 0; 0; 6;
         2; 4; 0; 0; 6; 9; 7; 0; 0;|]

   let sod = Soduku(rows)
   let b = sod.Complete
   sod.Solve
   0

