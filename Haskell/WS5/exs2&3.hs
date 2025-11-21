import Data.List (sortBy)

data Card = Card Face Suit
  deriving (Show, Eq)

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Show, Eq, Enum, Ord)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Ord)

allCards :: [Card]
allCards = [Card face suit | face <- [Two .. Ace], suit <- [Clubs .. Diamonds]]

-- Função de Comparação cmp1
-- Prioridade: 1º Suit (Naipe), 2º Face (Valor)
cmp1 :: Card -> Card -> Ordering
cmp1 (Card f1 s1) (Card f2 s2) =
  -- 1. Compara os Naipes (Suit)
  case compare s1 s2 of
    -- Se os naipes forem iguais (EQ), a ordem é definida pelo Face
    EQ -> compare f1 f2
    -- Caso contrário (LT ou GT), a ordem já está definida pelo Naipe
    x -> x

cmp2 :: Card -> Card -> Ordering
cmp2 (Card f1 s1) (Card f2 s2) =
  case compare f1 f2 of
    EQ -> compare s1 s2
    x -> x