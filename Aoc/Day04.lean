import Aoc
open List

def toListNum(s : String): List Nat :=
  s.splitOn " " 
    |> filter (λ x => not (["", " ", "  "].contains x))
    |> map (λ x => x.toNat!)

#eval toListNum "1  3 4 4"

def overlap(a b: List Nat): List Nat :=
  a.filter (b.contains ·)

def p1 (s: String): Nat:=
  let s := s.splitOn "\n" |> filter (λ s => not s.isEmpty)
  s.map (·.splitOn ": " |> (·.get! 1))
    |> map (λ s => s.splitOn " | ")
    |> map (λ s => (s.get! 0, s.get! 1))
    |> map (λ (one, two) => (toListNum one, toListNum two))
    |> map (λ (a, b) => overlap a b)
    |> filter (0 < ·.length)
    |> map (λ s => 2^(s.length - 1))
    |> foldl Nat.add 0

def addToList(i n x: Nat) (lis: List Nat): List Nat :=
  match n with 
    | 0 => lis
    | m+1 => 
      let idx := i + n
      if idx >= lis.length then
        addToList i m x lis
      else
        let val := lis.get! idx
        addToList i m x (lis.set idx (val+x))

#eval addToList 1 4 1 [1,1,1,1,1,1,1,1,1]
#eval addToList 1 4 2 [1,1,1,1,1]
#eval replicate 5 1

def p2 (s: String): Nat :=
  let s := s.splitOn "\n" |> filter (λ s => not s.isEmpty)
  let n := s.length
  let cards := s.map (·.splitOn ": " |> (·.get! 1))
    |> map (λ s => s.splitOn " | ")
    |> map (λ s => (s.get! 0, s.get! 1))
    |> map (λ (one, two) => (toListNum one, toListNum two))
    |> map (λ (a, b) => overlap a b)
    |> map (λ l => l.length)
  let wins := cards |> enum
    |> foldl (λ acc (idx, cardsWon) => 
      let val := acc.get! idx 
      addToList
        idx
        cardsWon 
        val
        acc
    ) (replicate n 1) 
  wins |> foldl Nat.add 0

#eval range 3
#eval 2^2

-- I think there is a bug in lean's parser, you can't name an identifier "matches"
-- def matches (a: Nat): Nat := a+1
--     ^^^^^^^ "unexpected token 'matches'; expected identifier"
-- or perhaps `matches` is a reserved word?

def main : IO Unit := do
  let input ← IO.readInput 1000000
  IO.println (p2 input)
