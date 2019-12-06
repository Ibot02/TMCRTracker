{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LogicParser (
    SettingsChoices (..),
    chooseDefaults,
    filterSettingsType,
    settingsChoices,
    parseLogicFile
) where

import Text.Parsec hiding ((<|>))
import qualified Text.Parser.Char as C
import qualified Text.Parser.Combinators as C
import Text.ParserCombinators.ReadP (readP_to_S, readS_to_P, ReadP())

import Data.Char (isSpace)
import Data.Maybe (listToMaybe)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map())
import qualified Data.Map as Map

import Control.Arrow (first, second)
import Control.Applicative ((<|>))
import Control.Monad (when)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State

import Data.Logic

type SettingType = String

data SettingsChoices m = SettingsChoices { chooseFlag :: SettingType -> String -> Bool -> m Bool, chooseDropdown :: SettingType -> (forall v. NonEmpty (String, v) -> m v) }

data Info = Info { infoName :: String, infoVersion :: String, infoCRC :: String } deriving (Eq, Ord, Show)

newtype StringWithReplacements = StringWithReplacements { getRaw :: String } deriving (Eq, Ord, Show)

instance (Monad m) => Stream StringWithReplacements (StateT (Map String String) m) Char where
    uncons (StringWithReplacements "") = return Nothing
    uncons (StringWithReplacements ('`':s)) = do
        let (name, '`':s') = span (/= '`') s
        replacement <- gets (Map.findWithDefault "" name)
        uncons $ StringWithReplacements $ replacement ++ s'
    uncons (StringWithReplacements (x:xs)) = return (Just (x,StringWithReplacements xs))

chooseDefaults :: (Applicative m) => SettingsChoices m
chooseDefaults = SettingsChoices (\_ _ b -> pure b) (\_ ((_, v) :| _) -> pure v)

filterSettingsType :: (SettingType -> Bool) -> SettingsChoices m -> SettingsChoices m -> SettingsChoices m
filterSettingsType p s s' = SettingsChoices (\t -> (chooseFlag $ if p t then s else s') t) (\t -> (chooseDropdown $ if p t then s else s') t)

settingsChoices :: (Applicative m) => (String -> Bool -> m Bool) -> (forall v. NonEmpty (String, v) -> m v) -> SettingsChoices m --only choose actual settings (not gimmicks)
settingsChoices cFlag cDropdown = filterSettingsType (== "Setting") (SettingsChoices (const cFlag) (const cDropdown)) chooseDefaults

parseLogicFile :: (Monad m) => SettingsChoices m -> SourceName -> String -> m (Either ParseError ([Rule], Info))
parseLogicFile c source input = flip evalStateT Map.empty $ runParserT p (Info "" "" "", []) source (StringWithReplacements input) where
            p = (,) <$> parseDirectives c <*> (fst <$> getState) <* eof

parseDirectives :: (Monad m, Stream s (StateT (Map String String) m) Char) => SettingsChoices m -> ParsecT s (Info, [Bool]) (StateT (Map String String) m) [Rule]
parseDirectives c = fmap concat $ many $ try (spaces' *> (blankLine <|> (directive <?> "a directive") <|> (rule <?> "a rule"))) where
    blankLine = [] <$ (spaces' *> try endOfLine') <?> "a blank line"
    -- ifs = do 
    --     inverted <- string "!if" *> option True (False <$ char 'n') <* string "def" <* sep
    --     argument <- value
    --     (m, (i, e)) <- getState
    --     let e' = inverted == Map.member argument m
    --     modifyState $ second $ second $ const $ e && e'
    --     thenBlock <- parseDirectives c
    --     modifyState $ second $ second $ const $ e && not e'
    --     spaces'
    --     elseBlock <- option [] $ try $ string "!else" *> endOfLine' *> parseDirectives c
    --     modifyState $ second $ second $ const e
    --     spaces'
    --     string "!endif" 
    --     endOfLine'
    --     return $ if e' then thenBlock else elseBlock
    directive = do
        char '!'
        dir <- many alphaNum
        args :: [String] <- option [] ( sep *> (option [] value `sepBy` try sep) <?> "arguments")
        (_, exec) <- getState
        case (dir, args) of
            ("define", n:v:_) -> when (and exec) $ lift $ modify $ Map.insert n v
            ("define", [n]) -> when (and exec) $ lift $ modify $ Map.insert n ""
            ("undefine", n:_) -> when (and exec) $ lift $ modify $ Map.delete n
            ("name", v:_) -> when (and exec) $ modifyState $ first $ \i -> i{infoName = v}
            ("version", v:_) -> when (and exec) $ modifyState $ first $ \i -> i{infoVersion = v}
            ("crc", v:_) -> when (and exec) $ modifyState $ first $ \i -> i{infoCRC = v}
            ("flag", flagType: flagName: flagDisplayName: "true": _) -> when (and exec) $ chooseFlag' flagType flagName flagDisplayName True
            ("flag", flagType: flagName: flagQuery: _) -> when (and exec) $ chooseFlag' flagType flagName flagQuery False
            ("dropdown", dropdownType: dropdownName : defaultChoiceDescr : defaultChoice : opts) -> when (and exec) $ do
                let opts' = makePairs opts
                choice <- lift $ lift $ chooseDropdown c dropdownType ((defaultChoiceDescr, defaultChoice) :| opts')
                lift $ modify $ Map.insert dropdownName choice
            ("ifdef", n:_) -> do
                cond <- lift $ gets (Map.member n)
                modifyState $ second (cond:)
            ("ifndef", n:_) -> do
                cond <- lift $ gets (not . Map.member n)
                modifyState $ second (cond:)
            ("else", _) -> modifyState $ second $ \(e:es) -> (not e:es)
            ("endif", _) -> modifyState $ second $ \(_:es) -> es
            (_,_) -> return ()
        endOfLine'
        return []
    rule = do
        locationName' <- value'
        locationRegion' <- option "" $ char ':' *> many alphaNum
        spaces'
        char ';'
        spaces'
        ruleType <- (RuleHelper <$ try (string "Helper")) <|> (RuleLocation <$ (try (string "Minor") <|> try (string "Major") <|> try (string "DungeonItem") <|> try (string "Unshuffled") <|> string "PurchaseItem")) --does PurchaseItem even still exist?
        spaces'
        char ';'
        skipMany $ noneOf ";\r\n#"
        char ';'
        spaces'
        ruleBody <- logicExpr
        optional value
        endOfLine'
        active <- and . snd <$> getState
        return [Rule (Location locationName' locationRegion') ruleType ruleBody | active]
    endOfLine' = spaces' *> (endOfLine <|> (char '#' *> many (noneOf "\r\n") *> endOfLine) <?> "a comment or end of line")
    spaces' = skipMany $ satisfy $ \c -> isSpace c && c `notElem` "\r\n"
    logicExpr = Conjunction <$> option [] clauses
    clauses = clause `sepBy1` try (spaces' *> char ',' *> spaces')
    clause = try conjunction <|> try disjunction <|> try count <|> primitive
    conjunction = fmap Conjunction $ between (string "(&" *> spaces') (spaces' *> char ')') clauses
    disjunction = fmap Disjunction $ between (string "(|" *> spaces') (spaces' *> char ')') clauses
    count = do
        string "(+"
        i <- read <$> many1 digit
        between (char ',' *> spaces') (spaces' *> char ')') $ fmap (Count i) $ thing `sepBy1` (spaces' *> char ',' *> spaces')
    primitive = do
        (x,c) <- thing
        return $ Count c [(x,1)]
    chooseFlag' flagType flagName flagQuery flagDefault = do
        b <- lift $ lift $ chooseFlag c flagType flagQuery flagDefault 
        lift $ modify $ if b then Map.insert flagName "" else Map.delete flagName
    sep = spaces' *> char '-' *> spaces'
    value = fmap (reverse . dropWhile isSpace . reverse) $ many1 (alphaNum <|> oneOf ";,_()&|<>.:/+ \t")
    value' = many1 (alphaNum <|> oneOf "_.")
    thing = do
        t <- ((Left .) . Item <$ string "Items.") <|> ((Right .) . Location <$ string "Helpers.") <|> ((Right .) . Location <$ string "Locations.")
        name <- value'
        (region, num) <- option ("",1) $ do
            char ':'
            region <- option "" value'
            num <- option 1 $ char ':' *> ((read <$> many1 digit) <|> (255 <$ string "FF"))
            return (region, num)
        return (t name region, num)

makePairs :: [a] -> [(a,a)]
makePairs (a:b:xs) = (a,b):makePairs xs
makePairs _ = []

-- valueToThing :: String -> Maybe (Thing, Int)
-- valueToThing = fmap fst . listToMaybe . readP_to_S (thingy <* C.eof) where
--         thingy :: ReadP (Thing, Int)
--         thingy = do
--             thingType <- (Item <$ C.string "Items.") <|> (Helper <$ C.string "Helpers.") <|> (Location <$ C.string "Locations.")
--             thingName <- C.many (C.alphaNum <|> C.oneOf "_." <|> (C.char ':' <* C.notFollowedBy (C.char ':')))
--             n <- C.option 1 $ C.string "::" *> (readS_to_P reads <|> (255 <$ C.string "FF"))
--             return (Thing thingType thingName, n)
