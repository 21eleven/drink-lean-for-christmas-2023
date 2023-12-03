import Aoc

def toN (x: List Char): Nat :=
  List.concat (x.take 1) x[x.length-1]!
  |> String.mk
  |> String.toNat!

def p1 (s: String): Nat:=
-- def p1 (s: String): List $ Nat:=
  s.splitOn "\n" 
  |> List.filter (·.length > 0)
  |> List.map String.toList
  |> List.map (List.filter Char.isDigit)
  |> List.map toN
  |> List.foldl Nat.add 0

-- def p2'(s: String): List Nat :=
--   s.replace "one" "1"
--   |> (·.replace "two" "2")
--   |> (·.replace "three" "3")
--   |> (·.replace "four" "4")
--   |> (·.replace "five" "5")
--   |> (·.replace "six" "6")
--   |> (·.replace "seven" "7")
--   |> (·.replace "eight" "8")
--   |> (·.replace "nine" "9")
--   -- s
--   |> p1


#eval "abcb".find λx => x == 'b'
-- def rep(s: String): String :=
--   String.find
--   Nat.fold (thingsThatParses) s.length


def wordHasPos(idxs: List Nat): Option Nat :=
  let pos := idxs.head!
  let indicesIndicateWordIsPresent :=
    idxs.enum 
    |> List.map (λ i => pos + i.1 == i.2)
    |> List.foldl (λ a b => a && b) true 
  match indicesIndicateWordIsPresent with
    | true => Option.some pos
    | false => Option.none

#eval wordHasPos [1, 2, 4]
#eval wordHasPos [1, 2, 3]
#eval wordHasPos [2, 3, 4]

#check "asdfgh".extract {byteIdx:=2} {byteIdx:=4}
#eval "asdfgh".extract {byteIdx:=2} {byteIdx:=4}


-- def findWord(a b: String): String :=
def findWord' (a b: String): Option Nat:=
  let bChars := b.toList
  bChars
  |> List.map (fun c => (a.find (fun x => x == c)).byteIdx)
  |> wordHasPos
  -- a ++ b


def findWord (a b : String) : Option Nat :=
  if b.isEmpty then none
  else
    let len := b.length
    let max := a.length - len + 1
    (List.range max).find? (λ i => a.extract {byteIdx:=i} {byteIdx:=i + len} == b)

def digitWordToDigit (word: String): String :=
  match word with
    | "one" => "o1e"
    | "two" => "t2o"
    | "three" => "t3e"
    | "four" => "f4r"
    | "five" => "f5e"
    | "six" => "s6x"
    | "seven" => "s7n"
    | "eight" => "e8t"
    | "nine" => "n9e"
    | _ => panic "wtf"

unsafe def replaceAll (ln: String) (maxReplace:Nat): String :=
  let digitsWords := ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  let foundWords := digitsWords.map 
    (λnum => (num, findWord ln num))
    |> List.filter λx => not (x.2 == Option.none)
  if foundWords.isEmpty then
    ln
  else
    let replaceTarget :=
      foundWords.foldl 
        λ (accNum, accIdx) (num, numIdx) =>
          match accIdx, numIdx with
            | some x, some y => if y < x then
                (num, numIdx)
              else (accNum, accIdx)
            | _, _ => (accNum, accIdx)
        ("acc", some ln.length)
      |> λ word => word.1

    replaceAll (ln.replace replaceTarget (digitWordToDigit replaceTarget)) (maxReplace - 1)
  -- termination_by replaceAll _ n => n 

#eval replaceAll "xxtwonezthree" 10
def p1' (s: String): List $ Nat:=
  s.splitOn "\n" 
  |> List.filter (·.length > 0)
  |> List.map String.toList
  |> List.map (List.filter Char.isDigit)
  |> List.map toN
  -- |> List.foldl Nat.add 0

unsafe def p2(s: String): Nat :=
  s.splitOn "\n" 
  |> List.filter (·.length > 0)
  |> List.map (replaceAll ·  300)
  |> List.map String.toList
  |> List.map (List.filter Char.isDigit)
  |> List.map toN
  |> List.foldl Nat.add 0


#eval replaceAll "xt36five77" 30
#eval replaceAll "two8five6zfrtjj" 30
#eval replaceAll "eightthree8fiveqjgsdzgnnineeight" 30
#eval replaceAll "7chmvlhnpfive" 30
#eval replaceAll "1tcrgthmeight5mssseight" 30
#eval replaceAll "eightoneqxspfzjk4zpfour" 30
#eval replaceAll "fdbtmkhdfzrck9kxckbnft" 30
#eval replaceAll "9six9" 30
#eval replaceAll "goneightczdzjk18589" 30
#eval replaceAll "41two3eightfscdmqjhdhtvsixld" 30
#eval replaceAll "t8929" 30
#eval replaceAll "fourtwoxsxqqmqf3sixfoursixmmjhdlx" 30
#eval replaceAll "bcbsfd14cjg" 30
#eval replaceAll "95three6threendpqpjmbpcblone" 30
#eval replaceAll "tdmvthreeonefive8574" 30
#eval replaceAll "tdmvthreeonefive8574" 30 |> String.toList |> List.filter Char.isDigit |> toN
#eval replaceAll "zoneight234" 30 |> String.toList |> List.filter Char.isDigit |> toN

#eval List.range 4
#eval findWord "Hello, world" "world"  -- Outputs: some 7
#eval findWord "Hello, world" "test"   -- Outputs: none
#eval findWord "aaonebbb" "one"

unsafe def main : IO Unit := do
  let input ← IO.readInput 1000000
  IO.println (p2 input)

