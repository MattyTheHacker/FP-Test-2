-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest2 (stateTrib, runStateTrib, writeLeaves, collapse, mapLeavesWithAddress, toQuadTree, fromQuadTree) where

import Data.List
import Data.Char

import Control.Monad.State
import Control.Monad.Writer

import Types

-- Question 1

stateTrib :: Integer -> State (Integer,Integer,Integer) ()
stateTrib = undefined

runStateTrib :: Integer -> Integer
runStateTrib n =
  let ((),(a,b,c)) = runState (stateTrib n) (1,0,0)
  in a

-- Question 2 DONE

-- Binary trees can have data at both the node and the leaves
-- perform an in-order traversal of the tree
-- log the values of the nodes and leaves using an Either type writer

writeLeaves :: Bin a b -> Writer [Either a b] ()
writeLeaves (Lf a) = tell [Left a]
writeLeaves (Nd b left right) = do
  writeLeaves left
  tell [Right b]
  writeLeaves right

-- Question 3 DONE

-- each leaf is decorated with binary trees
-- collapse the tree by removing the decoration

collapse :: Bin (Bin a b) b -> Bin a b
collapse (Lf (Lf a)) = Lf a
collapse (Lf (Nd b left right)) = Nd b (collapse (Lf left)) (collapse (Lf right))
collapse (Nd b left right) = Nd b (collapse left) (collapse right)

-- Question 4

-- map a function over the leaves of the tree
-- the function should be given the address of the leaf currently being processed
-- the address is a list of directions, L for left and R for right

mapLeavesWithAddress :: (a -> Address -> c) -> Bin a b -> Bin c b
mapLeavesWithAddress = undefined

getAddress :: Bin a b -> Address
getAddress (Lf _) = []
getAddress (Nd _ left right) = getAddress left ++ getAddress right


-- Question 5 DONE

-- a quad tree is a binary tree where each node has four children
-- a pixel is a represented by NW, NE, SW, SE
-- these tell us which quadrant of the image the pixel is in
-- the image is represented by a list of lists of pixels
-- the top left pixel is NW, the top right pixel is NE, etc
-- the two functions below should be the mutual inverse of each other


-- this function should take an image and convert it to a quad tree
-- the image is a list of lists of pixels
-- we an assume it is not empty and that the number of rows and columns are both powers of 2
toQuadTree :: Image -> QuadTree
toQuadTree image = do 
  (toQuadTree' image) (0, 0) (length image) (length image)

toQuadTree' :: Image -> (Int,Int) -> Int -> Int -> QuadTree
toQuadTree' image (x, y) width height
  | width == 1 && height == 1 = P (image !! x !! y)
  | otherwise = N (toQuadTree' image (x, y) (width `div` 2) (height `div` 2))
                  (toQuadTree' image (x, y + (height `div` 2)) (width `div` 2) (height `div` 2))
                  (toQuadTree' image (x + (width `div` 2), y) (width `div` 2) (height `div` 2))
                  (toQuadTree' image (x + (width `div` 2), y + (height `div` 2)) (width `div` 2) (height `div` 2))


-- this function should take a quad tree and convert it to an image
-- the image is a list of lists of pixels
-- we an assume it is not empty and that the number of rows and columns are both powers of 2
fromQuadTree :: QuadTree -> Image
fromQuadTree (P pixel) = [[pixel]]
fromQuadTree (N nw ne sw se) = zipWith (++) (fromQuadTree nw) (fromQuadTree ne) ++ zipWith (++) (fromQuadTree sw) (fromQuadTree se)