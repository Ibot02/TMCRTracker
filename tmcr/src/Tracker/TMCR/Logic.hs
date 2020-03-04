{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Tracker.TMCR.Logic where

import Data.Text (Text())
import qualified Data.Text as T

import qualified Data.Attoparsec.Text as P

import Text.Read (readMaybe)
import Data.Char
import Data.Map (Map())
import qualified Data.Map as Map
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens

import Tracker
import Tracker.Logic
import Control.Monad.Choice

type DungeonName = Text

data LogicFile = LogicFile {
    _logicFileName :: Text,
    _logicFileVersion :: Text,
    _logicFileContents :: [LogicFileContent]
    }
    deriving (Eq, Ord, Show)

data LogicFileContent = LogicFileBranch Text [LogicFileContent] [LogicFileContent]
                      | LogicFileDirective Text [Text]
                      | LogicFileRule Text
                      | LogicFileBlankLine
                      deriving (Eq, Ord, Show)

data LogicRule = LogicRule {
    _logicRuleHead :: (LocationName, DungeonName),
    _logicRuleType :: LogicRuleType,
    _logicRulePosition :: Text,
    _logicRuleExpression :: LogicExpr (Either ItemName LocationName),
    _logicRuleItemOverride :: Text
    }
    deriving (Eq, Ord, Show)

data LogicRuleType = Unshuffled | Helper | DungeonItem | Minor | Major
                   deriving (Eq, Ord, Enum, Show)

data Directive = Directive DirectiveType [Text]
               deriving (Eq, Ord, Show)
data DirectiveType = Eventdefine
                   | Replace
                   | ReplaceAmount
                   | ReplaceIncrement
                   | SetType
                   | Name
                   | Version
                   | CRC
                   deriving (Eq, Ord, Enum, Show)

preParseLogic :: Text -> Either Text LogicFile
preParseLogic input = runExcept $ fmap logicFile $ flip runStateT (Nothing, Nothing) $ foldr (\l a -> l >>= \l' -> a >>= helper l') (return [Left []]) $ fmap (branchPrefix . T.strip . T.takeWhile (not . (== '#'))) $ T.lines input where
    logicFile ([Left contents], (name, version)) = LogicFile (fromMaybe "" name) (fromMaybe "" version) contents
    helper (Right l) [] = throwError "Missing !endif"
    helper (Right l) (Left x:xs) = return $ Left (l:x):xs
    helper (Right l) (Right (x,y):xs) = return $ Right (l:x,y):xs
    helper (Left ("!endif":_)) xs = return $ Left [LogicFileBlankLine]:xs --add a blank line to make line numbers accurate in chooseSettings
    helper (Left ("!else":_)) (Left x:xs) = return $ Right ([LogicFileBlankLine],x):xs
    helper (Left ("!else":_)) (Right _:_) = throwError "Double !else"
    helper (Left ("!ifdef":arg:_)) (Left x:xs) = helper (Right (LogicFileBranch arg x [])) xs
    helper (Left ("!ifndef":arg:_)) (Left x:xs) = helper (Right (LogicFileBranch arg [] x)) xs
    helper (Left ("!ifdef":arg:_)) (Right (x,y):xs) = helper (Right (LogicFileBranch arg x y)) xs
    helper (Left ("!ifndef":arg:_)) (Right (x,y):xs) = helper (Right (LogicFileBranch arg y x)) xs
    branchPrefix text = 
        if "!" `T.isPrefixOf` text then
            if "!ifdef" `T.isPrefixOf` text || "!ifndef" `T.isPrefixOf` text || "!else" `T.isPrefixOf` text || "!endif" `T.isPrefixOf` text
            then return $ Left $ fmap T.strip $ T.splitOn "-" text
            else do
            when ("!name" `T.isPrefixOf` text) $ _1 .= (Just $ T.strip $ T.drop 1 $ snd $ T.breakOn "-" text)
            when ("!version" `T.isPrefixOf` text) $ _2 .= (Just $ T.strip $ T.drop 1 $ snd $ T.breakOn "-" text)
            return $ Right $ toDirective $ fmap T.strip $ T.splitOn "-" $ T.tail text
        else return $ Right $ if text == "" then LogicFileBlankLine else LogicFileRule text
    toDirective (d:args) = LogicFileDirective d args

chooseSettings :: forall m. (MonadChoice m) => LogicFile -> m (Either Text ([Directive], [LogicRule]))
chooseSettings (LogicFile _ _ contents) = runExceptT $ execWriterT $ flip runStateT (0, mempty) $ forM_ contents $ \l -> incrementLine *> run l where
    run :: LogicFileContent -> StateT (Int, Map Text Text) (WriterT ([Directive], [LogicRule]) (ExceptT Text m)) ()
    run (LogicFileRule r) = runReplacements r >>= rule >>= \r' -> tell (mempty, [r'])
    run (LogicFileDirective d args) = do
        d' <- runReplacements d
        args' <- forM args runReplacements
        case d' of
            "define" -> define' args'
            "undefine" -> undefine args'
            "flag" -> flag args'
            "dropdown" -> dropdown args'
            "color" -> color args'
            "numberbox" -> numberbox args'
            "addition" -> addition args'
            "eventdefine" -> tell ([Directive Eventdefine args'], mempty)
            "replace" -> tell ([Directive Replace args'], mempty)
            "replaceamount" -> tell ([Directive ReplaceAmount args'], mempty)
            "replaceincrement" -> tell ([Directive ReplaceIncrement args'], mempty)
            "settype" -> tell ([Directive SetType args'], mempty)
            "name" -> tell ([Directive Name args'], mempty)
            "version" -> tell ([Directive Version args'], mempty)
            "crc" -> tell ([Directive CRC args'], mempty)
            _ -> parserError $ "unknown directive !" <> d'
    run (LogicFileBranch condition t e) = do
        c <- runReplacements condition
        b <- gets $ Map.member c . snd
        if b then do
            forM_ t $ \l -> incrementLine *> run l
            skipLines e
        else do
            skipLines t
            forM_ e $ \l -> incrementLine *> run l
    run LogicFileBlankLine = return ()
    rule (fmap T.strip . T.splitOn ";" -> (name:t:pos:expr:item:[])) = rule' name t pos expr item
    rule (fmap T.strip . T.splitOn ";" -> (name:t:pos:expr:[])) = rule' name t pos expr ""
    rule (fmap T.strip . T.splitOn ";" -> (name:t:pos:[])) = rule' name t pos "" ""
    rule text = parserError $ "Could not parse rule:\n" <> text
    rule' name t pos expr item = do
        expr' <- parseExpr expr
        let (name', T.drop 1 -> dungeon) = T.breakOn ":" name
        t' <- parseRuleType t
        return $ LogicRule (name', dungeon) t' pos expr' item
    parseRuleType "Major" = return Major
    parseRuleType "Unshuffled" = return Unshuffled
    parseRuleType "Helper" = return Helper
    parseRuleType "DungeonItem" = return DungeonItem
    parseRuleType "Minor" = return Minor
    parseRuleType _ = parserError "Unknown Rule Type"
    parseExpr expr = case P.parseOnly (expressionParser <* P.endOfInput) expr of
        Left error -> parserError $ "Failed to parse expression: " <> expr <> "\n" <> T.pack error
        Right expr' -> return (expr' :: LogicExpr (Either ItemName LocationName))
    expressionParser = Conjunction <$> P.option [] expressionArgs
    expressionArgs = P.sepBy expressionParser' (P.string "," *> P.takeWhile isSpace)
    expressionParser' = conj <|> disj <|> count <|> fmap (\(name, count) -> Count count [(name,1)]) lit
    conj = fmap Conjunction $ P.string "(&" *> P.takeWhile isSpace *> expressionArgs <* P.takeWhile isSpace <* P.string ")"
    disj = fmap Disjunction $ P.string "(|" *> P.takeWhile isSpace *> expressionArgs <* P.takeWhile isSpace <* P.string ")"
    count = do
        P.string "(+"
        n <- P.hexadecimal
        P.takeWhile isSpace
        P.string ","
        P.takeWhile isSpace
        args <- P.sepBy1 lit (P.char ',' *> P.takeWhile isSpace)
        P.string ")"
        return $ Count n args
    lit = do
        f <- ((const Right <$> (P.string "Locations." <|> P.string "Helpers.")) <|> (const Left <$> P.string "Items."))
        name <- P.takeWhile isAlphaNum
        subtype <- P.option "" $ T.cons <$> P.char '.' <*> P.takeWhile isAlphaNum
        (dungeon, n) <- P.option ("",1) $ (,) <$> (cons <$> P.char ':' <*> P.takeWhile isAlphaNum) <*> P.option 1 (P.char ':' *> P.hexadecimal)
        P.takeWhile isSpace
        return $ (f (name <> subtype <> if dungeon /= ":" then dungeon else ""), n)
    runReplacements :: Text -> StateT (Int, Map Text Text) (WriterT ([Directive], [LogicRule]) (ExceptT Text m)) Text
    runReplacements i = do
        let (pre:parts) = T.splitOn "`" i
        paired <- getPairs' "Mismatched '`'" parts
        replaced <- forM paired $ \(ident, rest) ->
            fmap (<> rest) $ gets $ Map.findWithDefault "" ident . snd
        return $ foldl (<>) pre replaced
    incrementLine :: StateT (Int, Map Text Text) (WriterT ([Directive], [LogicRule]) (ExceptT Text m)) ()
    incrementLine = do
        l <- gets fst
        let l' = l + 1
        l' `seq` _1 .= l'
    skipLines lines = do
        l <- gets fst
        let l' = l + length lines
        l' `seq` _1 .= l'
    parserError msg = do
        l <- gets fst
        throwError $ msg <> "\nLine: " <> T.pack (show l)
    define name value = _2 . at name .= Just value
    define' [name, value] = define name value
    define' [name] = define name ""
    define' _ = parserError "Wrong number of arguments to !define"
    undefine [name] = _2 . at name .= Nothing
    undefine _ = parserError "Wrong number of arguments to !undefine"
    flag (scope:key:msg:r) = do
        d <- case r of
            [] -> return False
            [x] -> return $ x == "true"
            _ -> parserError "Wrong number of arguments to !flag"
        when (scope == "Setting") $ do
            c <- chooseFlag key msg d
            when c $ define key ""
    dropdown (scope: key: opts) = do
        kvs <- getPairs "Wrong number of arguments to !dropdown" opts
        if scope == "Setting" then do
            v <- chooseOption key kvs
            define key v
        else define key $ snd $ NE.head kvs
    getPairs' e [] = return []
    getPairs' e (k:v:opts) = fmap ((k,v):) $ getPairs' e opts
    getPairs' e _ = parserError e
    getPairs e (k:v:opts) = fmap ((k,v):|) $ getPairs' e opts
    getPairs e _ = parserError e
    numberbox (scope:key:msg:[]) = 
        if scope == "Setting" then do
            n <- chooseNumber key msg
            define key $ T.pack $ show n
        else define key "0"
    numberbox _ = parserError "Wrong number of arguments to !numberbox"
    color _ = return ()
    addition [key, vals] =
        case traverse (readMaybe . T.unpack . T.strip) $ T.splitOn "," vals of
            Nothing -> parserError "Non-numeric argument to !addition"
            Just ns -> define key $ T.pack $ show $ foldl' (+) (0 :: Int) ns
    addition _ = parserError "Wrong number of arguments to !addition"

getRules :: (MonadChoice m) => LogicFile -> m (Either Text [Rule (Either ItemName LocationName) LogicExpr LocationName])
getRules file = fmap (fmap (fmap toRule . snd)) $ chooseSettings file where

toRule :: LogicRule -> Rule (Either ItemName LocationName) LogicExpr LocationName
toRule rule = Rule (fst $ _logicRuleHead rule) (_logicRuleExpression rule)

getDungeonLocs :: [LogicRule] -> Map DungeonName [LocationName]
getDungeonLocs locs = Map.fromListWith (<>) [(dungeon, [loc]) | logicRule@(_logicRuleHead -> (loc, dungeon)) <- locs, _logicRuleType logicRule /= Helper]
