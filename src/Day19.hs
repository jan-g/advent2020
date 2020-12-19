module Day19 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))
import Control.Monad

import Lib

{-
--- Day 19: Monster Messages ---

You land in an airport surrounded by dense forest. As you walk to your high-speed train, the Elves at the Mythical Information Bureau contact you again. They think their satellite has collected an image of a sea monster! Unfortunately, the connection to the satellite is having problems, and many of the messages sent back from the satellite have been corrupted.

They sent you a list of the rules valid messages should obey and a list of received messages they've collected so far (your puzzle input).

The rules for valid messages (the top part of your puzzle input) are numbered and build upon each other. For example:

0: 1 2
1: "a"
2: 1 3 | 3 1
3: "b"

Some rules, like 3: "b", simply match a single character (in this case, b).

The remaining rules list the sub-rules that must be followed; for example, the rule 0: 1 2 means that to match rule 0, the text being checked must match rule 1, and the text after the part that matched rule 1 must then match rule 2.

Some of the rules have multiple lists of sub-rules separated by a pipe (|). This means that at least one list of sub-rules must match. (The ones that match might be different each time the rule is encountered.) For example, the rule 2: 1 3 | 3 1 means that to match rule 2, the text being checked must match rule 1 followed by rule 3 or it must match rule 3 followed by rule 1.

Fortunately, there are no loops in the rules, so the list of possible matches will be finite. Since rule 1 matches a and rule 3 matches b, rule 2 matches either ab or ba. Therefore, rule 0 matches aab or aba.

Here's a more interesting example:

0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

Here, because rule 4 matches a and rule 5 matches b, rule 2 matches two letters that are the same (aa or bb), and rule 3 matches two letters that are different (ab or ba).

Since rule 1 matches rules 2 and 3 once each in either order, it must match two pairs of letters, one pair with matching letters and one pair with different letters. This leaves eight possibilities: aaab, aaba, bbab, bbba, abaa, abbb, baaa, or babb.

Rule 0, therefore, matches a (rule 4), then any of the eight options from rule 1, then b (rule 5): aaaabb, aaabab, abbabb, abbbab, aabaab, aabbbb, abaaab, or ababbb.

The received messages (the bottom part of your puzzle input) need to be checked against the rules so you can determine which are valid and which are corrupted. Including the rules and the messages together, this might look like:

0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb

Your goal is to determine the number of messages that completely match rule 0. In the above example, ababbb and abbbab match, but bababa, aaabbb, and aaaabbb do not, producing the answer 2. The whole message must match all of rule 0; there can't be extra unmatched characters in the message. (For example, aaaabbb might appear to match rule 0 above, but it has an extra unmatched b on the end.)

How many messages completely match rule 0?

To begin, get your puzzle input.
-}

data Rule = Match Char
          | Seq [Rule]
          | Alt [Rule]
  deriving (Show, Eq)

-- parse :: [String] -> (Rule, [String])
parse ls =
  let [rules, input] = splitOn [""] ls
  in  (parseRules rules, input)

parseRules :: [String] -> Rule
parseRules rs =
  -- begin by parsing all rules into a set of (dependency -> {rules relying no this}) and (rule -> {dependencies})
  let fds = forwardDeps ruleIdToString
  -- resolve rules by parsing properly
      rules = expand Map.empty fds
  in rules Map.! 0
  where
    ruleIdToString :: Map.Map Integer String
    ruleIdToString = map splitRule rs & Map.fromList
    splitRule r = let [id, text] = splitOn ": " r in (read id, text)
    forwardDeps m = Map.map depsInRule ruleIdToString
    depsInRule :: String -> Set.Set Integer
    depsInRule s = splitWhen (not . isDigit) s & filter (/= "") & map read & Set.fromList
    expand :: Map.Map Integer Rule -> Map.Map Integer (Set.Set Integer) -> Map.Map Integer Rule
    expand currentRules fwds
      | Map.size fwds == 0 = currentRules
      | otherwise =
        let resolved = Map.filter (\r -> Set.size r == 0) fwds & Map.keysSet  :: Set.Set Integer
            -- for each of those, parse the rules and fold them in
            rs' = foldr (\k rs ->
                          let newRule = parseRule (ruleIdToString Map.! k) rs
                          in  Map.insert k newRule rs) currentRules resolved  :: Map.Map Integer Rule
            -- remove those elements from the forward rules
            fwds' = Map.filterWithKey (\k _ -> not $ Set.member k resolved) fwds
                  & Map.map (\fs -> Set.difference fs resolved)
        in  expand rs' fwds'
    parseRule s rs = quickParse ((matchParser <++ altParser rs <++ seqParser rs) <<<< eof) s & fromJust

matchParser :: ReadP Rule
matchParser = do
  (do
    char '"'
    c <- get
    char '"'
    return $ Match c)

altParser :: Map.Map Integer Rule -> ReadP Rule
altParser currentRules = do
  rs <- sepBy1 (seqParser currentRules) (string " | ")
  if length rs > 1 then pure (Alt rs) else pure (head rs)

seqParser :: Map.Map Integer Rule -> ReadP Rule
seqParser currentRules = do
  rns <- sepBy1 natParser (char ' ')
  return $ Seq [currentRules Map.! rn | rn <- rns]


matches rule start =
  let suffixes = match rule start
  in  Set.member "" suffixes

-- Does a rule match a string?
-- input: the current suffix we're matching
-- result: the set of suffixes remaining after matching
match :: Rule -> String -> Set.Set String

match (Match _) "" = Set.empty
match (Match c) (x:xs)
  | x == c = Set.singleton xs
  | otherwise = Set.empty

match (Alt as) xs = Set.unions [match a xs | a <- as]

match (Seq ss) xs = seqMatch ss xs
  where
    seqMatch :: [Rule] -> String -> Set.Set String
    seqMatch [] xs = Set.singleton xs
    seqMatch (r:rs) xs =
      let rest = match r xs   :: Set.Set String
          matches = Set.map (seqMatch rs) rest
      in  Set.unions matches


day19 ls =
  let (rule, inputs) = parse ls
  in  filter (matches rule) inputs & length

{-
--- Part Two ---

As you look over the list of messages, you realize your matching rules aren't quite right. To fix them, completely replace rules 8: 42 and 11: 42 31 with the following:

8: 42 | 42 8
11: 42 31 | 42 11 31

This small change has a big impact: now, the rules do contain loops, and the list of messages they could hypothetically match is infinite. You'll need to determine how these changes affect which messages are valid.

Fortunately, many of the rules are unaffected by this change; it might help to start by looking at which rules always match the same set of values and how those rules (especially rules 42 and 31) are used by the new versions of rules 8 and 11.

(Remember, you only need to handle the rules you have; building a solution that could handle any hypothetical combination of rules would be significantly more difficult.)

For example:

42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba

Without updating rules 8 and 11, these rules only match three messages: bbabbbbaabaabba, ababaaaaaabaaab, and ababaaaaabbbaba.

However, after updating rules 8 and 11, a total of 12 messages match:

    bbabbbbaabaabba
    babbbbaabbbbbabbbbbbaabaaabaaa
    aaabbbbbbaaaabaababaabababbabaaabbababababaaa
    bbbbbbbaaaabbbbaaabbabaaa
    bbbababbbbaaaaaaaabbababaaababaabab
    ababaaaaaabaaab
    ababaaaaabbbaba
    baabbaaaabbaaaababbaababb
    abbbbabbbbaaaababbbbbbaaaababb
    aaaaabbaabaaaaababaa
    aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
    aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba

After updating rules 8 and 11, how many messages completely match rule 0?
-}

day19b ls =
  -- let's just cheat on this
  let [rules, input] = splitOn [""] ls
  in  addN 2 rules input (day19 ls)
  where
    addN n rules input last =
      -- Note: this isn't perfect. There could be a string that's matched by 5 iterations of "8: 41 41 41 41 41" but nothing by
      -- four iterations, "8: 41 41 41 41" in which case we'll stop hunting early.
      -- As it is, this data doesn't meet that rule.
      -- A better way to deal with this would be to add matching rules that deal with positive closure
      -- and bracketed repeats, perhaps
      let eights = intercalate " | " (take n $ repeatsOfString " " "42")
          elevens = intercalate " | " (take n $ zipWith (\a b -> a ++ " " ++ b) (repeatsOfString " " "42") (repeatsOfString " " "31"))
          rules' = rules ++ ["8: " ++ eights] ++ ["11: " ++ elevens]
          results = day19 $ rules' ++ [""] ++ input
      in  if results == last then results
          else addN (n + 1) rules input results


repeatString n sep str = repeat str & take n & intercalate sep

repeatsOfString sep str = [str] ++ [str ++ sep ++ s | s <- repeatsOfString sep str]

