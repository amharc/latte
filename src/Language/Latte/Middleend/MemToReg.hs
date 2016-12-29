{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Latte.Middleend.MemToReg (opt) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.Coerce
import Data.Foldable
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Language.Latte.Middleend.IR
import Language.Latte.Middleend.Monad

opt :: (MonadIO m, MonadState s m, HasMiddleEndState s) => m ()
opt = use meFunctions >>= mapM_ runFunction

runFunction :: (MonadIO m, MonadState s m, HasMiddleEndState s) => FunctionDescriptor -> m ()
runFunction desc = do
    blocks <- reachableBlocks $ desc ^. funcEntryBlock
    matchables <- getMatchables blocks
    startNames <- replicateM (length blocks) . fmap Map.fromList $
         forM matchables $ \matchable ->
             (matchable,) <$> mkName (Just $ matchableName matchable)
    let startValues = startNames & traverse . traverse %~ OperandNamed
    endValues <- zipWithM runBlock blocks startValues
    preds <- predecessors blocks
    let phiBranches block = Map.fromList
            [ ( matchable
              , [PhiBranch pred (values Map.! matchable) | pred <- preds Map.! block | values <- endValues]
              )
            | matchable <- matchables
            ]
    zipWithM_ (addPhiNodes =<< phiBranches) blocks startNames
    resetEntryBlock (head startNames) $ desc ^. funcEntryBlock

runBlock :: MonadIO m => Block -> Bindings -> m Bindings
runBlock block start = do
    instructions <- liftIO . readIORef $ block ^. blockBody
    let (instructions', end) = foldl (flip runInstruction) (Seq.empty, start) instructions
    liftIO $ writeIORef (block ^. blockBody) instructions'
    pure end

addPhiNodes :: MonadIO m => Map.Map Matchable [PhiBranch] -> Block -> Map.Map Matchable Name -> m ()
addPhiNodes branches block bindings = flip itraverse_ bindings $ \matchable name ->
    let node = PhiNode name (branches Map.! matchable) in
    liftIO $ modifyIORef' (block ^. blockPhi) (Seq.|> node) 

resetEntryBlock :: (MonadIO m) => Map.Map Matchable Name -> Block -> m ()
resetEntryBlock matchables block = do
    liftIO $ writeIORef (block ^. blockPhi) Seq.empty
    iforM_ matchables $ \matchable name ->
        case matchable of
            MatchableLocal _ _ -> emit (Instruction name (IConst OperandUndef) [])
            MatchableArgument size idx -> emit (Instruction name (Load (MemoryArgument idx) size) [])
  where
    emit instr = liftIO $ modifyIORef' (block ^. blockBody) (instr Seq.<|)

runInstruction :: Instruction -> (Seq.Seq Instruction, Bindings) -> (Seq.Seq Instruction, Bindings)
runInstruction i@(view instrPayload -> Load from size) (instrs, matchables)
    | Just matchable <- getMatchable size from
    , Just operand <- Map.lookup matchable matchables
    = (instrs Seq.|> (i & instrPayload .~ IConst operand), matchables)
runInstruction (view instrPayload -> Store to size value) (instrs, matchables)
    | Just matchable <- getMatchable size to
    = (instrs, Map.insert matchable value matchables)
runInstruction i@(view instrPayload -> Inc arg size) (instrs, matchables)
    | Just matchable <- getMatchable size arg
    , Just operand <- Map.lookup matchable matchables
    = ( instrs Seq.|> (i & instrPayload .~ BinOp operand BinOpPlus (OperandInt 1))
      , Map.insert matchable (OperandNamed $ i ^. instrResult) matchables
      )
runInstruction i@(view instrPayload -> Dec arg size) (instrs, matchables)
    | Just matchable <- getMatchable size arg
    , Just operand <- Map.lookup matchable matchables
    = ( instrs Seq.|> (i & instrPayload .~ BinOp operand BinOpMinus (OperandInt 1))
      , Map.insert matchable (OperandNamed $ i ^. instrResult) matchables
      )
runInstruction instr (instrs, matchables) = (instrs Seq.|> instr, matchables)

type Bindings = Map.Map Matchable Operand

data Matchable = MatchableLocal Size Int | MatchableArgument Size Int
    deriving (Eq, Ord, Show)

matchableName :: Matchable -> Ident
matchableName (MatchableLocal _ i) = coerce $ BS.concat ["local", BS.pack $ show i]
matchableName (MatchableArgument _ i) = coerce $ BS.concat ["arg", BS.pack $ show i]

getMatchables :: MonadIO m => [Block] -> m [Matchable]
getMatchables blocks = toList <$> foldM goBlock Set.empty blocks
  where
    goBlock acc block = do
        body <- liftIO (readIORef $ block ^. blockBody)
        pure $ foldl goInstr acc body

    goInstr acc instr = case instr ^. instrPayload of
        Load from size -> goMemory size from acc
        Store to size _value -> goMemory size to acc
        Inc arg size -> goMemory size arg acc
        Dec arg size -> goMemory size arg acc
        _ -> acc

    goMemory size mem acc = case getMatchable size mem of
        Nothing -> acc
        Just matchable -> Set.insert matchable acc

getMatchable :: Size -> Memory -> Maybe Matchable
getMatchable size (MemoryLocal loc) = Just $ MatchableLocal size loc
getMatchable size (MemoryArgument loc) = Just $ MatchableArgument size loc
getMatchable _ _ = Nothing
