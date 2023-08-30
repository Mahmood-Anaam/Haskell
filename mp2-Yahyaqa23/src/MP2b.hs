module MP2b where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GHC.Float (int2Float, float2Int)



-- Makes a barren world for the Game of Life. A world is a 2-tuple of:
--  1. its width (W) and height (H), as a tuple of integers
--  2. a list of H lists of W Bools, each value representing a cell's state
--     (True for living, False for dead)
-- jklreflkhrjjhj
makeWorld :: (Int,Int)  -- (width,height) of world
          -> ((Int,Int), [[Bool]])  -- world
makeWorld dims@(w,h) = (dims, replicate h $ createCell w)


createCell 0=[]
createCell w
       | even w  = True:createCell (w-1)
       | otherwise =False:createCell (w-1)

-- Computes the number of living neighbors of a cell.
liveNeighbors :: ((Int,Int), [[Bool]])  -- world
              -> (Int,Int)  -- cell
              -> Int  -- num of living neighbors



convertB2I :: Bool->Int
convertB2I b= if b then 1 else 0 
convertI2B :: Int->Bool
convertI2B n= n >0 
sumNevghbors ::Int->[Bool]->Int
sumNevghbors index lst=
            let r= if (index+1) >= length lst then 0 else convertB2I (lst !! (index+1))
                l=if (index-1) < 0 then 0 else convertB2I (lst !! (index-1))
            in r+l+convertB2I (lst !! index)

liveNeighbors ((w,h), cells) (r,c)= 
            let rt= if (r-1)<0 then 0 else sumNevghbors c (cells !! (r-1))
                rm=sumNevghbors c (cells !! r) - convertB2I ((cells !! r)!!c)
                rb=if (r+1)>=h then 0 else sumNevghbors c (cells !! (r+1))
            in rt+rm+rb


-- Computes the next world state according to Conrad's rules:
--  1. Birth: a dead cell with exactly three living neighbors comes alive.
--  2. Survival: a living cell with two to three neighbors remains alive. 
--  3. Death: a living cell with zero or one neighbors dies in isolation; 
--            a living cell with four or more neighbors dies of overcrowding


nextWorld :: ((Int,Int), [[Bool]])  -- current world
          -> ((Int,Int), [[Bool]])  -- next world


nextRow ::(Int, [[Bool]])->[Bool]->Int->[Bool]
nextRow (w,cells) row rindex
            | w==0 =[]
            | not (row !! (w-1)) = if ng==3 then True:nextRow (w-1,cells) row rindex else False:nextRow (w-1,cells) row rindex
            | otherwise = if ng>3 || ng<2 then False:nextRow (w-1,cells) row rindex else True:nextRow (w-1,cells) row rindex
             where ng= liveNeighbors ((length (head cells),length cells), cells) (rindex,w-1)




nextWorld ((w,h),cells)= ((w,h),getcellsrs h cells)
    where getcellsrs he cellse
               | he==0 = []
               | otherwise = [nextRow (w, cellse)  (cellse!!(he-1)) (he-1)] ++ (getcellsrs (he-1) cellse)


-- Draw a picture of the world
drawWorld :: ((Int,Int), [[Bool]])  -- world
          -> Picture
          
drawWorld ((w,h),cells)=
        let ws=int2Float w
            hs=int2Float h
            n= ws*hs
            rsize=5
            psize=1

           in scale 1 1$
                translate (-(ws*(rsize+psize))/2) (-(hs*(rsize+psize))/2) $
                   pictures (foldr (++) [] [ getRrowcells i 0 rsize psize (cells !! float2Int i) | i <- [0..hs-1] ])
                     

getRrowcells:: Float->Float->Float->Float->[Bool]->[Picture]
getRrowcells _ _ _ _ []=[]
getRrowcells rindex cindex rsize psize (x:xs)=(getRcell (cindex*(rsize+psize)) (rindex*(rsize+psize)) rsize x):getRrowcells rindex (cindex+1) rsize psize xs


getRcell :: Float -> Float -> Float -> Bool -> Picture
getRcell sx sy rsiz state =translate sx sy (color (if state then black else white) (rectangleSolid rsiz rsiz))



-- Handle an event affecting the world. The only event we handle is a mouse
-- click, which will create life in the targeted cell.
handleEvents :: Event  -- event information
             -> ((Int,Int), [[Bool]])  -- world
             -> ((Int,Int), [[Bool]])
handleEvents (EventKey (MouseButton LeftButton) Up _ (mx,my)) 
             world@((w,h), cells) 
    = let r= round (int2Float h/2 ) + round (my/6)
          c= round (int2Float w/2 ) + round (mx/6)
      in ((w,h),changCell ((w,h), cells) (r,c))

handleEvents _ world = world

changCell ::((Int,Int), [[Bool]]) -> (Int,Int)-> [[Bool]]


updateRow :: (Eq t, Num t) => [Bool] -> t -> [Bool]
updateRow [] _=[]
updateRow (re:row) inc
         |inc==0 = True:updateRow row (inc-1)
         |otherwise =re:updateRow row (inc-1)

changCell ((w,h), cells) (r,c)=changRows 0 (r,c) cells



changRows _ _ []=[]
changRows he (r,c) (x:xs)
        | he==r = [updateRow x c]++(changRows (he+1) (r,c) xs)
        | otherwise =[x]++(changRows (he+1) (r,c) xs)








