-- | DESIGN AND STRATEGY DOCUMENTATION
{-
-- ==========================================================================================

  ------ SPOILER ALERT ------
  Important defintions used throughout the documentation:
  1) winning: refers to avoiding taking the trick (to avoid as much point cards as possible)
  2) losing: refers to aiming to take the trick (to take as much point cards as possible)
  3) hand: refers to cards on my hand at the moment
  4) tricks: refers to the cards played in a turn
  5) void: refers to an emptying a slot as soon as possible 


  ----- OVERALL STRATEGY -----
  Overall game strategy is split into leading and reneging as below.

  1) Leading
    [A] I have Two of Clubs
      Action: Throw Two of Clubs
      Reason: To comply with game rules

    [B] Moon is predicted to be shot
      Action: Choose the card that could take the trick
      Reason: Strategy will be switched to aiming for taking the trick, to minimize the chances of opponents collecting subsequent point cards to shoot the moon.

    [C] All other situations
      Action: Choose the smallest card that could create a void
      Reason: Strategy will be switched to always letting the opponents to take the trick by throwing the smallest card to create a void. This will have higher chance of them taking the lead in subsequent rounds and I could renege them accordingly.

  2) Reneging
    => This is where most strategies reside in.
    => The checkings are done in the following orders:
      
    [A] Having the leading suit
      1) The first round of the game
        Action: Throw the highest card of the leading suit because the 
        Reason: Chances of opponents throwing a point card in the first round is mild, so it is safe to eradicate highest card.

      2) Moon is predicted to be shot
        Action: Choose the card that could take the trick
        Reason: Strategy will be switched to aiming for taking the trick, to minimize the chances of opponents collecting subsequent point cards to shoot the moon.

      3) Queen of Spades in current trick
        Action: Choose the card that is lowest than the largest leading card in the trick (a safe card)
        Reason: To avoid taking the trick as Queen of Spades has the highest point card. 

      4) Queen of Spades in previous tricks
        Action: Choose the card with highest probability of winning (of avoid taking the trick)
        Reason: It is now safe to throw the cards which I will not take the trick, such as if I have anything higher than Queen of Spades, it is safe to throw now.
    
    [B] No leading suit
      => It is now safe for me to remove all high point cards as possible
      1) The first round of the game
        Action: Throw the highest card to create a void. 
        Reason: Chances of first round having a point card thrown is mild. 
      
      2) I have Queen of Spades
        Action: Throw Queen of Spade
        Reason: To remove high point cards as early as possible when I have a chance to do so.

      3) I have Hearts
        Action: Throw Highest Heart card
        Reason: Remove as high point cards as possible and to make opponents to take the point cards.

      4) No more Hearts
        Action: Throw highest card to create a void
        Reason: Maximize my chance of removing high cards of risking in the late game. A void slot gives me advantage of throwing point cards when I don't have a leading suit.


  ----- OVERALL DESIGN -----
  Overall game design is split into parts as below.

  1) Cards Playing Section
    => Most of the logics of cards filtering happen here. 
    => Guards are used to filter out different conditions to result in cleaner code and fluency in readability and also makes it easier to debug problems when tracing them out. 
    => Probabilistic cards are simulated here based on the list of filtered cards so that it could be passed as the list of cards to be chosen / picked by the (4) Cards Picking Section
    => When a certain condition happen, it will call functions from the (4) Cards Picking Section to perform a selection of the card

  2) Helper Functions Section
    => Small and reusable functions are located here
    => These small functions are generalized to adapt to wide variety of situations so that they are reusable and more elegant and not result in code redundancy
    => These small functions can be proven to be reusable when they are used by (1) Cards Playing Section
    => They are also safe because the the returned result from these functions are always clean and pure.
    => These usages of safe functions offer referential transparency, where the function does not change its overall behaviour of the program (single responsibility principle)

  3) Memory Manipulating Section
    => Memory is manipulated / parsed in this section.
    => It takes the previous memory state and extract all sorts of information from the memory string such as cards played in previous rounds and players' scores.
    => It is generalized to take in the previous state information (Maybe type) so that it could be called by anywhere to extract information by only utilizing the previous state, making it more simple and elegant.

  4) Players' Scores Manipulating Section
    => Players scores are updated in this region.

  5) Cards Simulating Section
    => Cards such as cards to be voided, probabilistic cards are simulated here.
    => Most of the time, major functions in this section takes in the hand and returns a new hand to be played
    => For instance, voidCards takes in a hand and returns a list of cards to be voided. Or simulateProbabilities which generates the probabilities of each card. 
    => These returned hand will then be used by (1) Cards Playing Section to be the hand of cards to be played, creating safe hands and cleaner code 

  6) Cards Picking Section
    => Cards are picked here according to various functionalities. 
    => Functions located here don't deal with filering cards but it only deals with choosing a card from a list of hand cards given, creating cleaner code. 
  
  ----- REFERENCES -----
  Learnt about hearts game from:
  https://www.247hearts.com
  https://www.alanhoyle.com/hearts-copy-strategy.html
  https://viphearts.com/blog/advanced-strategies-in-hearts/

  Learn about functional programming haskell from:
  http://learnyouahaskell.com/a-fistful-of-monads
  http://learnyouahaskell.com/syntax-in-functions#guards-guards
  http://learnyouahaskell.com/functors-applicative-functors-and-monoids
  https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/pattern-and-guard-extensions#patternguards

  Learnt about programming principles such as clean code from:
  https://wiki.haskell.org/Programming_guidelines
  https://softwareengineering.stackexchange.com/questions/254631/applying-the-principles-of-clean-code-to-functional-languages
-- ==========================================================================================
-}

module Player (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Cards
import Data.Maybe
import Data.List
import Hearts.Types
import Data.List
import Data.Ord
import Data.Map (fromList, toList, adjust)
import Data.Function


-- ==========================================================================================
{--
    -- CARDS PLAYING SECTION --
    Functions residing in this section have logics / conditions for certain strategy
    to be performed. Most of the time cards are filtered along the way and 
    when a certain strategy is fulfilled, card is picked from the list of cards 
    by calling functions from the Card Picking Section.
--}
-- ==========================================================================================


playCard :: PlayFunc
playCard myId hand [] previousState = (lead myId hand previousState, getNewMemory previousState)
playCard myId hand tricks previousState = (renege myId hand (map fst tricks) previousState, getNewMemory previousState)


lead :: PlayerId -> [Card] -> Maybe([(Card, PlayerId)], String) -> Card
lead myId hand previousState 
  -- throw the two of clubs if i have it
  | haveCard hand twoOfClubs = twoOfClubs
  -- if shoot moon is detected, get the card which has the minimal win (aka switch strategy to take the trick) (optimized for 2 players)
  | willShootMoon = getMinWin probabilitiesHand Nothing
  -- otherwise, void the smallest card
  | otherwise = getSmallestVoidCard filteredHand
  where
    -- obtains the list of cards in previous rounds 
    previousCards = convertMemory previousState ++ convertTricks previousState
    -- obtains the list of cards to play that complies with bleeding rule
    filteredHand = safeFilter (not $ haveSuit previousCards Heart) ((Heart/=).getSuit) hand
    probabilitiesHand = simulateProbabilities filteredHand [] previousState

    playerScores = getPlayerScores previousState
    willShootMoon = predictMoonShooting myId playerScores (length playerScores)



renege :: PlayerId -> [Card] -> [Card] -> Maybe ([(Card, PlayerId)], String) -> Card
renege _ hand currentCards Nothing
  -- throw the largest card of i have the leading suit in the first round
  | haveLeadingSuit = getLargestCard bleedHand leadSuit
  -- otherwise throw the largest void card to create a void (empty slot)
  | otherwise = getLargestVoidCard bleedHand
  where
    leadSuit = getSuit $ last currentCards
    bleedHand = safeFilter True (\x -> x /= queenOfSpade && getSuit x /= Heart) hand
    haveLeadingSuit = haveSuit bleedHand leadSuit

renege myId hand currentCards previousState 
  -- take the trick if moon is predicted to be shot
  | haveLeadingSuit && willShootMoon = getLargestCard filteredHand leadSuit
  
  -- if there is a Queen now or Queen has not been thrown, play safe prevent throwing highest card 
  | haveLeadingSuit && (currentHaveQueen || not previousHaveQueen) = getLargestSafeCard filteredHand maxCurrentCard 
  
  -- if Queen has been thrown, can throw high cards now
  | haveLeadingSuit && previousHaveQueen = getMaxWin probabilitiesHand (Just leadSuit)
  
  -- if I have Queen
  | handHaveQueen = queenOfSpade
  
  -- if I have hearts, throw the highest, to reduce risk
  | handHaveHearts = getMaxWin probabilitiesHand (Just Heart)
  
  -- no hearts, switch strategy to create void - emptying suits with least number of cards, whether hearts have been broken or not, goal will still be to create void based on the cards (breakHand) that I have
  | otherwise = getLargestVoidCard breakHand

  where 
    leadSuit = getSuit $ last currentCards
    haveLeadingSuit = haveSuit hand leadSuit
    previousCards = convertMemory previousState ++ convertTricks previousState
    maxCurrentCard = maximum $ filterSuit currentCards leadSuit

    -- filter hand based on leading suit
    filteredHand = safeFilter haveLeadingSuit ((leadSuit==).getSuit) hand
    
    -- conditions checking if the list of cards has the stuffs that i am looking for
    handHaveQueen = haveCard hand queenOfSpade
    handHaveHearts = haveSuit hand Heart
    previousHaveQueen = haveCard previousCards queenOfSpade
    currentHaveQueen = haveCard currentCards queenOfSpade
    previousHaveHearts = haveSuit previousCards Heart
    currentHaveHearts = haveSuit currentCards Heart
    heartsBroken = previousHaveHearts || currentHaveHearts

    -- filter hand based on hearts broken
    breakHand = safeFilter (not (heartsBroken)) ((Heart/=).getSuit) hand 
    
    -- simulate probabilities of each card
    probabilitiesHand =  simulateProbabilities filteredHand currentCards previousState

    -- check if moon is predicted to be shot (for 2 players)
    playerScores = getPlayerScores previousState
    willShootMoon = predictMoonShooting myId playerScores (length playerScores)


-- ==========================================================================================
{--
    -- HELPER FUNCTIONS SECTION --
    Functions residing in this section are small, reusable helper functions to perform 
    various kind of powerful operations by the caller. These small functions are used 
    because they are:
    1) simple and easier to understand
    2) easier to debug and test 
    3) easier to combine with larger functions to perform heavy operations
      (such as functions in Cards Simulating Section and Cards Picking Section)
--}
-- ==========================================================================================


-- | Returns the Two of Club
-- Input: -
-- Output: Card Club Two
twoOfClubs :: Card
twoOfClubs = Card Club Two


-- | Returns the Queen of Spade
-- Input: -
-- Output: Card Spade Queen 
queenOfSpade :: Card
queenOfSpade = Card Spade Queen


-- | Returns the suit of a card
-- Input: Card Spade Queen 
-- Output: Spade
getSuit :: Card -> Suit
getSuit (Card s _) = s


-- | Returns the rank of a card
-- Input: Card Spade Queen 
-- Output: Queen
getRank :: Card -> Rank
getRank (Card _ r) = r


-- | Returns if the cards has the given suit
-- Input: [Card Spade Queen, Card Spade Jack] Spade 
-- Output: True
haveSuit :: [Card] -> Suit -> Bool
haveSuit cards suit = any (==suit) (getSuit <$> cards)


-- | Returns if the cards has the given card
-- Input: [Card Spade Queen, Card Spade Jack] (Card Spade Queen) 
-- Output: True
haveCard :: [Card] -> Card -> Bool
haveCard cards card = card `elem` cards


-- | Filters the cards of the given suit and returns the resulting list of card(s)
-- Input: [Card Spade Queen, Card Spade Jack, Card Heart Five] Heart 
-- Output: [Card Heart Five]
filterSuit :: [Card] -> Suit -> [Card]
filterSuit cards suit = filter ((suit==).getSuit) cards


-- | Filters safely the cards by applying the function given and returns the original cards changed if the filtered cards are empty, otherwise returns the filtered cards result.
-- Input: True ((==Heart).getSuit) [Card Heart Five, Card Heart Ten] 
-- Output: [Card Heart Five, Card Heart Ten]
safeFilter :: Bool -> (Card -> Bool) -> [Card] -> [Card]
safeFilter False _ cards = cards
safeFilter True f cards = if length newCards == 0 then cards else newCards
  where newCards = filter f cards


-- ==========================================================================================
{--
    -- MEMORY PARSING / SPLITTING / MANIPULATING SECTION --
    Information in the previous's state (aka: Maybe ([(Card, PlayerId)], String)) is 
    manipulated according to different needs, such as from extracting the list of cards 
    played in all previous rounds to extracting each player's score information.
    Functions residing in this section usually takes in the previous's state information 
    as input to be parsed correspondingly (other than the helper functions). 
--}
-- ==========================================================================================


-- | Splits string according to the delimiter character given
-- | Inspired from: https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- Input: '|' "abc|123" 
-- Output: ["abc", "123"]
splitBy :: Char -> String -> [String]
splitBy d s = case dropWhile (==d) s of
                    "" -> []
                    s'-> x : splitBy d s''
                      where (x, s'')= break (==d) s'


-- | Splits the main memory delimiter by a pipe
-- Input: "H5 H6 H7 H8 H9 H10 HJ HQ|P1:5 P2:5 P3:0 P4:1"
-- Output: ["H5 H6 H7 H8 H9 H10 HJ HQ","P1:5 P2:5 P3:0 P4:1"]
splitMemory :: String -> [String]
splitMemory memory = splitBy '|' memory


-- | Splits the parts (elements) by a space
-- Input: "H5 H6 H7 H8 H9 H10 HJ HQ"
-- Output: ["H5","H6","H7","H8","H9","H10","HJ","HQ"]
splitParts :: String -> [String]
splitParts parts = splitBy ' ' parts


-- | Split the players' scores by a colon
-- Input: "P1:5"
-- Output: ["P1","5"]
splitScores :: String -> [String]
splitScores scores = splitBy ':' scores


-- | Converts the tricks into a list of cards
-- Input: Just([(Card Spade Queen, "P1"), (Card Spade King, "P2"), (Card Spade Ace, "P3"), (Card Spade Jack, "P4")], "")
-- Output: [Card Spade Queen, Card Spade King, Card Spade Ace, Card Spade Jack]
convertTricks :: Maybe ([(Card, PlayerId)], String) -> [Card]
convertTricks Nothing = []
convertTricks (Just (tricks, _)) = map fst tricks


-- | Converts the memory into a list of cards
-- Input: Just([(Card Spade Queen, "P1"), (Card Spade King, "P2"), (Card Spade Ace, "P3"), (Card Spade Jack, "P4")], "H5 H6 H7 H8|P1:5 P2:5 P3:0 P4:1")
-- Output: [Card Heart Five, Card Heart Six, Card Heart Seven, Card Heart Eight]
convertMemory :: Maybe ([(Card, PlayerId)], String) -> [Card]
convertMemory Nothing = []
convertMemory (Just (_, "")) = []
convertMemory (Just (_, memory)) = parseString $ splitParts $ head $ splitMemory memory
  where
    -- | Parses a list of strings (string representation of each card) into a list of cards
    -- Input: "H5 H6 H7 H8"
    -- Output: [Card Heart Five, Card Heart Six, Card Heart Seven, Card Heart Eight]
    parseString :: [String] -> [Card]
    parseString [] = []
    parseString string = map (\x -> read x :: Card) string


-- | Converts the memory into a list of players' scores. This will be represented as a list of tuples of type (playerId, score), if games are yet to be played, each player's score will be zero. Note: this convertScores function does not return the updated scores, i.e., where the scores have not updated for previous tricks. It only extract the scores of each player exists in the string memory.
-- Input: Just([(Card Spade Queen, "P1"), (Card Spade King, "P2"), (Card Spade Ace, "P3"), (Card Spade Jack, "P4")], "H5 H6 H7 H8|P1:5 P2:5 P3:0 P4:1")
-- Output: [("P1",5),("P2",5),("P3",0),("P4",1)]
convertScores :: Maybe([(Card, PlayerId)], String) -> [(PlayerId, Int)]
convertScores Nothing = []
convertScores (Just (tricks, "")) = zip (map snd tricks) (replicate (length tricks) 0) 
convertScores (Just (_, memory)) = map ((\[a, b] -> (a, read b :: Int)) . splitScores) $ splitParts $ last $ splitMemory memory


-- | Returns a new string along with the tricks updated. This string will contain two information: 
-- 1) the cards played all previous rounds - where each card is separated with a space ' '
-- 2) the scores of all players - where individual player is separated with a space ' ' and the player's score is separated with a colon ':'
-- These two information are stored in the string, separated with a pipe '|' to allow easy split in subsequent rounds. 
-- Input: Just([(Card Spade Queen, "P1"), (Card Spade King, "P2"), (Card Spade Ace, "P3"), (Card Spade Jack, "P4")], "H5 H6 H7 H8|P1:4 P2:0 P3:0 P4:0")
-- Output: "H5 H6 H7 H8 SQ SK SA SJ|P1:4 P2:0 P3:13 P4:0"
getNewMemory :: Maybe ([(Card, PlayerId)], String) -> String
getNewMemory Nothing = ""
getNewMemory previousState = newMemory ++ "|" ++ newScores
  where
    newMemory = intercalate " " (map (show) (convertMemory previousState ++ convertTricks previousState))
    newScores = intercalate " " (map (parseScore) (getPlayerScores previousState))
    -- | Returns a string representation of the player's score, separated with a colon ':'
    parseScore :: (PlayerId, Int) -> String
    parseScore (playerId, score) = playerId ++ ":" ++ show score 


-- ==========================================================================================
{--
    -- PLAYERS SCORES CONTROLLER / MANIPULATING SECTION -- 
    Players' scores are updated in this section such as getPlayerScore returns a list 
    of updated scores for each player.  
--}
-- ==========================================================================================


-- | Returns the updated list of players' scores (including points in previous round) by taking all the points of the point cards in previous trick and add them to the player's score who took the trick (of previous turn). 
-- Input: Just([(Card Spade Queen, "P1"), (Card Spade King, "P2"), (Card Spade Ace, "P3"), (Card Spade Jack, "P4")], "H5 H6 H7 H8|P1:4 P2:0 P3:0 P4:0")
-- Output: [("P1",4),("P2",0),("P3",13),("P4",0)]
getPlayerScores :: Maybe([(Card, PlayerId)], String) -> [(PlayerId, Int)]
getPlayerScores Nothing = convertScores Nothing
-- adds the score to the player who took the trick
getPlayerScores a@(Just (tricks, _)) = toList $ adjust (+gameScore) winner (fromList $ convertScores a)
  where
    leadSuit = getSuit $ fst $ last tricks
    -- obtain a list of previous tricks with only leading suit
    filteredLead = filter ((leadSuit==) . getSuit . fst) tricks
    -- obtain the score (points) of all point cards in previous turn
    gameScore =  sum $ map getCardPoints $ convertTricks a
    -- obtain the player id of the player who took the trick
    winner = snd $ maximumBy (comparing fst) filteredLead

    getCardPoints :: Card -> Int
    getCardPoints (Card Spade Queen) = 13
    getCardPoints (Card Heart _) = 1
    getCardPoints _ = 0


-- | Returns if the moon is predicted to be shot (only for 2-players mode)
-- Input: "P1" [("P1",0),("P2",12)] 2
-- Output: False
predictMoonShooting :: PlayerId -> [(PlayerId, Int)] -> Int -> Bool
predictMoonShooting _ _ 4 = False
predictMoonShooting myId playerScores _ = any (>=13) $ map snd $ filter ((/=myId).fst) playerScores


-- ==========================================================================================
{--
    -- CARDS SIMULATING SECTION --
    Cards are generated or filtered accordingly. 
    Functions residing in this section usually return a list of cards to be played
    (other than helper / small functions).
--}
-- ==========================================================================================


-- | Generate all the cards from a hand (to create a void - empty slot) by searching for the cards which suit has the least quantity. This is achieved by calculating the number of cards for each suit and returns the list of cards of suit with minimal amount. 
voidCards :: [Card] -> [Card]
-- the returned list of suits (of minimal amount) is binded with a function that filters out the hand cards according to the suit so that it returns a list of cards to be played (of the minimal suit)
voidCards hand = filteredVoids >>= (filterSuit hand).fst
  where 
    -- simulate the number of cards for each suit in the hand
    simulatedVoids = filter ((>0) . snd) $ calculateSuit <$> [Spade ..]
    -- filteredVoids will return a list of suits that have the minimal amount (so that it can be voided easily to create empty slots)
    filteredVoids = head $ groupBy (on (==) snd) $ sortBy (comparing snd) simulatedVoids

    -- | Returns a pair of tuple with the given suit and the number of cards with that suit
    calculateSuit :: Suit -> (Suit, Int)
    calculateSuit suit = (suit, length $ filterSuit hand suit)


-- | Returns a list of remaining cards that have not been played in the game by taking in the cards that have been played (which include the player's hand, cards in current tricks and cards in previous turns).
remainingCards :: [Card] -> [Card]
remainingCards played = ((Card <$> [Spade ..] <*> [Two ..]) \\ played)


-- | Returns the number of cards yet to be played in a trick by finding the difference between the number of cards played in previous turn and the number of cards played in current turn. If no game is played (first round), Nothing is returned.
-- Input: (Just([(Card Spade Queen, "P1"), (Card Spade King, "P2"), (Card Spade Ace, "P3"), (Card Spade Jack, "P4")], "H5 H6 H7 H8|P1:5 P2:5 P3:0 P4:1")) [Card Heart Two] 
-- Output: Just 3
getTrickSize :: Maybe([(Card, PlayerId)], String) -> [Card] -> Maybe Int
getTrickSize Nothing _ = Nothing
getTrickSize (Just (tricks, _)) currentCards = Just (length tricks - length currentCards)


-- | Simulate all possible combinations of cards according to the size given
-- | Inspired from: https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell
-- Input: [Card Spade Queen, Card Spade Jack, Card Heart Five] 2
-- Output: [[Card Spade Queen, Card Spade Jack], [Card Spade Queen, Card Heart Five], [Card Spade Jack, Card Heart Five]]
simulateCombinations :: [Card] -> Int -> [[Card]]
simulateCombinations _ 0 = return []
simulateCombinations cards size = do  y:cards' <- tails cards
                                      ys <- simulateCombinations cards' (size-1)
                                      return (y:ys)


-- | Returns True the given card lose the trick (aka not taking the trick - winning) and False otherwise (losing)
-- Input: (Card Spade Eight) Spade [Card Spade Nine, Card Heart Five, Card Spade Two, Card Spade Three]
-- Output: True
winLose :: Card -> Suit -> [Card] -> Bool
winLose card leadSuit simulatedPair = not $ (leadSuit == getSuit card) && (getLargestCard (card:simulatedPair) leadSuit == card)


-- | Simulate a list of cards paired with its probabilities (chances), it first finds the list of remaining cards to be played (which are the cards owned by the opponent(s)) and simulate every possible combination. Each card in the hand is then mapped into all combinations of cards (including the cards in the current turn) to obtain the total possible number of winnings. 
-- Note: the term "winning" here is defined as losing trick (not taking the trick/turn) because it minimizes the probability of taking point cards. A high number associated with the card indicates that the card has high chance of not taking the trick. 
simulateProbabilities :: [Card] -> [Card] -> Maybe([(Card, PlayerId)], String) -> [(Card, Int)]
simulateProbabilities _ _ Nothing = []
simulateProbabilities hand currentCards previousState = map getWinPoints hand
  where
    -- obtain the list of cards owned by the opponent(s) 
    balanceCards = remainingCards (hand ++ currentCards ++ convertMemory previousState ++ convertTricks previousState)
    -- obtain the size of the each combination of cards to be simulated (-1 to exclude my card)
    size = (fromJust (getTrickSize previousState currentCards)) - 1
    -- obtain the list of simulated combinations (which also includes the cards in current trick)
    simulatedCards = map ((++) currentCards) $ simulateCombinations balanceCards size

    -- | Returns the chances (probability) of a card
    getWinPoints :: Card -> (Card, Int)
    getWinPoints card = (card, length $ filter (==True) $ map (winLose card (getLeadSuit currentCards card)) simulatedCards)

    -- | Returns the leading suit of the current tricks. If no cards in current tricks, the card to be discarded itself is the leading suit.
    getLeadSuit :: [Card] -> Card -> Suit
    getLeadSuit [] card = getSuit $ card
    getLeadSuit cards _ = getSuit $ last cards


-- ==========================================================================================
{--
    -- CARDS PICKING SECTION -- 
    Card is picked here according to different strategies.
    Functions residing in this section usually directly returns the card to be played to 
    the caller of the function.
--}
-- ==========================================================================================


-- | A general function that folds a list into the result of the element type by applying foldl with the function given. A reusable function that allows applying onto a function given and folding onto the entire list. 
fold :: (a -> a -> a) -> [a] -> a
fold f list = foldl f (head list) list

-- | A general function that takes in a comparison function that compares the rank of two cards
compareRank :: (Rank -> Rank -> Bool) -> Card -> Card -> Card
compareRank f a b = if f (getRank b) (getRank a) then b else a 

-- | Returns the card with the highest probability of winning by looking for the card which has the highest win rate.
-- | Inspired from: https://stackoverflow.com/questions/24921738/filter-list-of-tuples-by-maximum-element
-- Input: [(Card Spade Five, 15), (Card Spade Ten, 5), (Card Heart Ace, 15)] (Just Spade)
-- Output: Card Spade Five
getMaxWin :: [(Card, Int)] -> Maybe Suit -> Card
getMaxWin handPairs Nothing = fst . maximumBy (comparing snd) $ handPairs
getMaxWin handPairs (Just suit) = fst . maximumBy (comparing snd) $ filter ((suit==) . getSuit . fst) handPairs


-- | Returns the card with the lowest probability of winning by looking for the card which has the lowest win rate. 
-- | Inspired from: https://stackoverflow.com/questions/24921738/filter-list-of-tuples-by-maximum-element
-- Input: [(Card Spade Five, 15), (Card Spade Ten, 5), (Card Heart Ace, 15)] (Just Spade)
-- Output: Card Spade Ten
getMinWin :: [(Card, Int)] -> Maybe Suit -> Card
getMinWin handPairs Nothing = fst . minimumBy (comparing snd) $ handPairs
getMinWin handPairs (Just suit) = fst . minimumBy (comparing snd) $ filter ((suit==) . getSuit . fst) handPairs


-- | Returns the smallest card that could create a void (to make a suit void) by comparing the rank of each card and returns the smallest rank card.
-- Input: [Card Spade King, Card Spade Queen, Card Heart Two, Card Heart Ace]
-- Output: Card Heart Two
getSmallestVoidCard :: [Card] -> Card
getSmallestVoidCard hand = fold (compareRank (<)) (voidCards hand)


-- | Returns the largest card that could create a void (to make a suit void) by comparing the rank of each card and returns the largest rank card. 
-- Input: [Card Spade King, Card Spade Queen, Card Heart Two, Card Heart Ace]
-- Output: Card Heart Ace
getLargestVoidCard :: [Card] -> Card
getLargestVoidCard hand = fold (compareRank (>)) (voidCards hand)


-- | Takes in a list of filtered cards and find the largest card that is smaller than the card given. This card chosen is the safest card to throw.
-- Input: [Card Spade Ace, Card Spade Queen, Card Heart Two] (Card Spade King)
-- Output: Card Spade Queen
getLargestSafeCard :: [Card] -> Card -> Card
getLargestSafeCard hand card = foldl (\a b -> if b > a && b < card then b else a) (minimum hand) hand


-- | Returns the largest card in the list of cards by filtering out the suit and gets the maximum card.
-- Input: [Card Spade Ace, Card Spade King, Card Heart Five] Spade
-- Output: Card Spade Ace
getLargestCard :: [Card] -> Suit -> Card
getLargestCard hand suit = maximum (filterSuit hand suit)



-- | Not used, do not remove
makeBid :: BidFunc
makeBid = undefined
