import Aoc
open List

def makeAdjPositions(pos len: Nat): List Nat :=
  let subList := [1, len, len - 1, len + 1]
  let positions := subList.map (λs => 
    if s <= pos then
      some (pos - s)
    else none
  ) |> filter Option.isSome |> map Option.get!
  [pos + 1, pos + len -1, pos + len, pos + len + 1] ++
  positions 

#eval makeAdjPositions 11 5
#eval makeAdjPositions 3 5
#eval makeAdjPositions 0 5
#eval makeAdjPositions 1 5
 
def chk (s: String): Nat := 
  let chk := 1
  sorry

def parse(data: List (Nat × Char)) (acc: List ((List Nat) × Nat)) (fuel: Nat): List ((List Nat) × Nat) :=
  -- "fuel" hack to avoid unsafe or manually proving termination
  match fuel with
    | 0 => acc
    | x+1 =>
      -- peel off first group of non digits if any
      let (_, remain₁) := data.span λ (_, c) => not (c.isDigit)
      -- peel off number
      let (number₀, remain₂) :=  remain₁.span λ (_, c) => (c.isDigit)
      if number₀.length == 0
        then acc
      else
        let number := number₀.map (λx => x.2) |> String.mk |> String.toNat!
        let indicies := number₀.map (λx => x.1)
        parse remain₂ (concat acc (indicies, number)) x
  

def listsShareElement {α : Type} [DecidableEq α] : List α → List α → Bool
| [], _ => false
| _, [] => false
| (x :: xs), ys => if x ∈ ys then true else listsShareElement xs ys

#eval span (not $ Char.isDigit ·) "..35..633.".toList
#eval parse ("..35..633.".toList |> enum) [] 100000

def p1 (s: String): Nat :=
  let lns := s.splitOn "\n" 
  let len := head! lns |> (·.length)
  let oneStr := lns.foldl (λ a b => a ++ b.toList) [] |> enum
  let symPos := oneStr
    |> filter (λ(_, s) => "!@#$%^&*()/=-_+".contains s)
    |> map (λx => x.1)
  let numberPos := symPos |> map (makeAdjPositions · len) |> foldl List.append []
  let numbers := parse oneStr [] 1000000
  numbers |> filter (λ(idxs, _) => listsShareElement idxs numberPos)
    |> map (λx => x.2)
    |> foldl Nat.add 0

def p2 (s: String): Nat := 
  let lns := s.splitOn "\n" 
  let len := head! lns |> (·.length)
  let oneStr := lns.foldl (λ a b => a ++ b.toList) [] |> enum
  let symPos := oneStr
    |> filter (λ(_, s) => "*".contains s)
    |> map (λx => x.1)
  let numbers := parse oneStr [] 1000000
  symPos.map (λstarIdx => 
      let area := makeAdjPositions starIdx len
      numbers.filter (λ(idxs, _) => listsShareElement idxs area)
    )
    |> filter (λstar => star.length==2)
    |> map (λ nums => 
      match nums with 
        | [(_, a), (_, b)] => a * b
        | _ => panic "wtf"
    )
    |> foldl Nat.add 0

def main : IO Unit := do
  let input ← IO.readInput 1000000
  IO.println (p2 input)
  
