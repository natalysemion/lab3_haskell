{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
data Transition = Transition
  { fromState :: String,
    toState :: String,
    symbol :: String
  }

data FiniteAutomaton = FiniteAutomaton
  { states :: [String],
    symbols :: [String],
    transitions :: [Transition],
    startState :: String,
    finalStates :: [String]
  }

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = lines <$> readFile filePath

parseTransition :: String -> Transition
parseTransition line =
  let [fromState, toState, symbol] = words line
   in Transition fromState toState symbol

-- Перевірка, чи існує слово довжиною k, що допускається автоматом
acceptsWordOfLengthK :: FiniteAutomaton -> Int -> Maybe String
acceptsWordOfLengthK fa k = searchStrings fa (startState fa) "" Nothing k

-- Пошук першого слова довжиною k, яке може бути прийняте автоматом
searchStrings :: FiniteAutomaton -> String -> String -> Maybe String -> Int -> Maybe String
searchStrings _ _ currentString foundWord k
  | length currentString > k = foundWord
searchStrings fa currentState currentString foundWord k
  | length currentString == k && currentState `elem` finalStates fa = Just currentString
  | otherwise =
      case foundWord of
        Just _ -> foundWord
        Nothing ->
          foldr
            ( \transition acc ->
                if fromState transition == currentState
                  then searchStrings fa (toState transition) (currentString ++ symbol transition) acc k
                  else acc
            )
            Nothing
            (transitions fa)

main :: IO ()
main = do
  -- Зчитування даних про автомат з файлів
  states <- readLinesFromFile "states.txt"
  symbols <- readLinesFromFile "symbols.txt"
  finalStates <- readLinesFromFile "final_states.txt"
  transitionsContent <- readLinesFromFile "transitions2.txt"
  let transitions = map parseTransition transitionsContent
  let startState = head states

  -- Створення об'єкта FiniteAutomaton
  let automaton = FiniteAutomaton states symbols transitions startState finalStates

  putStrLn "Enter the length of the word (k):"
  k <- readLn

  -- Перевірка, чи існує слово довжиною k, що допускається автоматом
  let result = acceptsWordOfLengthK automaton k

  -- Виведення результату
  case result of
    Just word -> putStrLn $ "The automaton accepts the word \"" ++ word ++ "\" of length " ++ show k ++ "."
    Nothing -> putStrLn $ "The automaton does not accept any word of length " ++ show k ++ "."
