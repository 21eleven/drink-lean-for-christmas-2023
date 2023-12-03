import Aoc

structure Hand where
  green: Nat := 0
  red: Nat := 0
  blue: Nat := 0

instance : Inhabited Hand where
  default := {}
--
instance : ToString Hand where
  toString s := "{g: " ++ toString s.green ++ " r: " ++ toString s.red ++ " b: " ++ toString s.blue ++ "}"

structure Game where
  id : Nat 
  hands: List Hand

instance : ToString Game where
  toString g := "[" ++ toString g.id ++ "]" ++ toString g.hands

def makeHand (hand: String): Hand :=
  hand.splitOn ", "
  |> List.filter (λs => not s.isEmpty)
  |> List.foldl (fun (h: Hand) (s: String) =>
    let g := h.green
    let r := h.red
    let b := h.blue
    let val := (s.splitOn " ")[0]!.toNat!
    let color := (s.splitOn " ")[1]!
    match color with 
      | "green" => {green:=g+val,red:=r,blue:=b}
      | "red" => {green:=g,red:=r+val,blue:=b}
      | "blue" => {green:=g,red:=r,blue:=b+val}
      |  _ => panic "wtf") {}


#check makeHand "10 red, 3 blue"
#eval makeHand "10 red, 3 blue"

def isHandPossible(hand: Hand): Bool :=
  if hand.green <= 13 && hand.red <= 12 && hand.blue <= 14 
    then true else false 

def isGamePossible(game: Game): Nat :=
  let possible := game.hands.map isHandPossible 
    |> List.foldl (λ acc handPossible => acc && handPossible) true
  if possible then game.id else 0

def p1 (games: String): Nat:=
  games.splitOn "\n"
  |> List.filter (λs => not s.isEmpty)
  |> List.map (λs => (String.splitOn s ": ")[1]!)
  |> List.map (String.splitOn · "; ")
  |> List.map (λround => List.map makeHand round)
  |> List.enumFrom 1
  |> List.map (λ x => {id:=x.1,hands:=x.2})
  |> List.map isGamePossible
  |> List.foldl Nat.add 0

def minSet(g: Game): Hand :=
  let minGreen := match g.hands.map (λh => h.green) |> List.maximum? with
    | some n => n
    | none => 0
  let minRed := match g.hands.map (λh => h.red) |> List.maximum? with
    | some n => n
    | none => 0
  let minBlue:= match g.hands.map (λh => h.blue) |> List.maximum? with
    | some n => n
    | none => 0
  {green:=minGreen,red:=minRed,blue:=minBlue}

def powerSet(h: Hand): Nat :=
  h.green * h.red * h.blue

def p2 (games: String): Nat:=
  games.splitOn "\n"
  |> List.filter (λs => not s.isEmpty)
  |> List.map (λs => (String.splitOn s ": ")[1]!)
  |> List.map (String.splitOn · "; ")
  |> List.map (λround => List.map makeHand round)
  |> List.enumFrom 1
  |> List.map (λ x => {id:=x.1,hands:=x.2})
  |> List.map minSet
  |> List.map powerSet
  |> List.foldl Nat.add 0

def main : IO Unit := do
  let input ← IO.readInput 1000000
  IO.println (p2 input)
