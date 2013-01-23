module Music where

import Data.Maybe (mapMaybe)

import Euterpea 

type KeyN = (Int -> Rational -> Music Pitch, Int)
type MMusic = ([Int], [Rational], [KeyN])
      
--here are a bunch of sample transformations. To make custom transformations,
--enter them here and change ctList1 to contain your transformations      
      
type CTransform = (String, Int)
type CTransList = [CTransform]
      
ct0, ct1, ct2, ct3, ct4, ct5, ct6, ct7, ct8, ct9, ct10, ct11, ct12, ct13, ct14,
    ct15 :: CTransList

ct0 = [("Pivot", 2)]
ct1 = [("Move", 2)]
ct2 = [("Switch Mode", 0)]
ct3 = [("Switch Dur", 0)]      
ct4 = [("Cadence", 0)]
    
ct5 = [("sMove", 3),("Pivot", 4),("Switch Dur", 0)]
ct6 = [("Switch Mode", 0),("sMove", -2)]
ct7 = [("Cadence", 0)]    
ct8 = [("sMove", -1),("Switch Dur", 1)]

ct9 = [("sMove", 3)]
ct10 = [("Switch Mode", 0)]
ct11 = [("sMove", -2)]
ct12 = [("sMove", -1)]

ct13 = [("sMove", 2)]
ct14 = [("Switch Mode", 0)]
ct15 = [("sMove", -1),
        ("Pivot", 3)]
ct16 = [("sMove", -1)]

ctList1 = [ct13, ct14, ct15, ct16]   

cInitial :: MChord
cInitial = fMinC  

aMajC, fMajC, fMinC :: MChord

aMajC = (0,1,"p1","root",(9,"Maj",3))
fMajC = (0,1,"p1","root",(5,"Maj",4))  
fMinC = (0,1,"p1","root",(5,"Min",3))  
     
--this will convert the MMusic to Music Pitch
--The key portion of MMusic is be (note-function, octave)
--then all you have to do is pick out of the key and apply the dur

mMusToMPitch :: MMusic -> Music Pitch

mMusToMPitch (mus,dur,key) = 
    if mus == [] then rest 0 
    else mNumToMPitch (head mus) (head dur) key :+:
            mMusToMPitch ((tail mus),(tail dur),key)
            
--mNumToMPitch: given n, dur, and key, make a Music Pitch that is the nth
--note of key with duration dur
           
mNumToMPitch :: Int -> Rational -> [KeyN] -> Music Pitch

mNumToMPitch shft dur key = 
    let fullOcts = quot shft (length key) 
        octShift = if shft < 0 then fullOcts - 1 else fullOcts
        index =  mod shft (length key)
        (nt, oct) = key!!index
        newOct' = oct + octShift
        newOct = if newOct' < 0 then 0 
                 else if newOct' > 10 then 10
                      else oct + octShift
    in nt newOct dur            
            
--and to do a whole list of them            
            
mMusListToPitch :: [MMusic] -> Music Pitch
mMusListToPitch [] = rest 0
mMusListToPitch (m1:ms) = mMusToMPitch m1 :+: mMusListToPitch ms       
        
-- scale building

--chromatic scale at octave n
notesN :: Int -> [KeyN]
notesN n = [(c, n), (cs, n), (d, n), (ds, n), (e, n), (f, n), (fs, n), (g, n), 
          (gs, n), (a, n), (as, n), (b, n)]

notes4 :: [KeyN]
notes4 = notesN 4       
       
--build scale can build a major or minor scale at a specified octave and root              
       
buildScale :: Int -> String -> Int -> [KeyN]

buildScale n typ oct =         
    let bsh [] = []
        bsh (l1:ls) = let (nm, o) = (notesN oct)!!l1
                      in if l1 < n then (nm, o+1) : bsh ls
                         else (nm, o) : bsh ls
    in bsh $ map (flip mod 12 . (+) n) (getScaleEq typ)
  where 
    getScaleEq s = 
      case s of
        "Maj" -> [0,2,4,5,7,9,11]
        "Min" -> [0,2,3,5,7,8,10]
        otherwise -> getScaleEq "Maj"
        
          
----------------------------CHORDS----------------------------------------------

{- here are some ideas for chord transformations:

1. treat as pivot 
2. relative major/minor
3. num -> num

this makes me think we want a chord structure that contains a chord number, a 
duration, a duration pattern, a type, and a key

the key itself will be a root, mode, and octave
-}

type MChord = (Int, Rational, String, String, (Int, String, Int))

mMusToChord :: MMusic -> Music Pitch

mMusToChord(mus,dur,key) = 
    if mus == [] then rest 0
    else mNumToMPitch (head mus) (head dur) key :=:
            mMusToChord ((tail mus),(tail dur),key)
            
mChordToPitch :: MChord -> Music Pitch

--Note: only good for 7 note scales right now. Could adapt in the future

mChordToPitch (nte, dur, pattern, typ, (rt, mode, octv)) =
    let chordEq = map (+ nte) $ getChordEq typ
    in mMusToChord (chordEq, map (*dur) $ durPattern (length chordEq) pattern, 
                    buildScale rt mode octv)  
  where
    getChordEq s =
        case s of
            "root" -> [0,2,4]
            "six" -> [2,4,7]
            "six-four" -> [4,7,9]
            "seven" -> [0,2,4,6]
            "six-five" -> [2,4,6,7]
            "four-three" -> [4,6,7,9]
            "two" -> [6,7,9,11]
            "sixth" -> [0,2,4,5]
            otherwise -> getChordEq "root"
    durPattern n s =
        case s of
            "p1" -> extendList [1] n
            "p2" -> extendList [1, 1/2] n
            "p3" -> extendList [1, 1/2, 1/4] n
            otherwise -> durPattern n "p1"

--we need the following two functions to make the length patterns viable for 
--all chords
            
repList :: [a] -> Int -> [a]
repList ls' n = let rlh _ cur 1 = cur
                    rlh init cur n = rlh init (cur ++ init) (n-1)
                in rlh ls' ls' n
    
extendList :: [a] -> Int -> [a]

extendList [] _ = []
extendList lst n = take n $ repList lst (ceiling (fromIntegral n / 
                                                  fromIntegral (length lst)))

--Cadence will be treated separately, since it has type MChord -> [MChord]

transChord :: MChord -> CTransList -> MChord

transChord c [] = c
transChord c (t1:ts) = transChord (applyCTrans t1 c) ts
    where
        applyCTrans (s, n) =
            case s of
                "Pivot" -> flip pivotChord n
                "Move" -> flip moveChord n
                "sMove" -> flip moveSmooth n
                "Switch Mode" -> switchMode 
                "Scale Dur" -> flip scaleDur (toDur n)
                "Switch Dur" -> flip switchDur n
        toDur x = if x < 0 then 1 / (-1*fromIntegral x) else fromIntegral x
        
transToChordList :: MChord -> [CTransList] -> [MChord]
transToChordList c [] = []
transToChordList c ([("Cadence", n)]:ts) =
    let doCad = cCadence c (cadenceTypes!!n)
    in doCad ++ transToChordList (doCad!!2) ts
  where
    cadenceTypes = [(0.5, 0.5, 1.0),(0.25,0.75,0.5),(0.667,0.667,0.667)] 
transToChordList c (t1:ts) = 
    let doTrans = transChord c t1
    in [doTrans] ++ transToChordList doTrans ts
    
--here's a function to make the durList that Display needs
--takes transformations, outputs their lengths in seconds    
    
mkCDurList :: [Int] -> [Float]
mkCDurList ns' = 
    let cads = findCadences ctList1
        trans = numsToChordList ns'
        getDur (_,d,_,_,_) = 2.0*fromRational d
        dlh [] _ = []
        dlh (n1:ns) (t1:ts) = 
            let (newD,newTs) = if elem n1 cads then 
                                (getDur t1 + getDur (head ts) + 
                                 getDur (head (tail ts)), (tail (tail ts)))
                               else (getDur t1, ts)
            in newD : dlh ns newTs
    in getDur cInitial : dlh ns' trans

findCadences :: [CTransList] -> [Int]
findCadences cs' = 
    let fch [] _ = []
        fch (c1:cs) ct = if fst (head c1) == "Cadence" then ct : fch cs (ct+1)
                         else fch cs (ct+1)
    in fch cs' 0    
    
cListToPitch :: [MChord] -> Music Pitch
cListToPitch [] = rest 0
cListToPitch (c1:cs) = mChordToPitch c1 :+: cListToPitch cs

numsToChordList :: [Int] -> [MChord]
numsToChordList numList = cInitial : transToChordList cInitial 
                                        (map ((!!) ctList1) numList)

numsToChords :: [Int] -> Music Pitch
numsToChords numList = cListToPitch $ numsToChordList numList

playChords :: [Int] -> IO ()
playChords numList = play (instrument RhodesPiano (numsToChords numList))

--auxiliary transformation functions
    
{- The pivot transform is interesting. Keeping a strictly conventional system 
of major and minor triads, there are 3 major, 3 minor triads and 1 diminished 
triad for every major or minor. That means that each major or minor triad could be 
reinterpreted in two other keys of the same type and 3 keys of the other type, 
while diminished triads can only be re-interpreted as a triad in one key of the
other type. Thus, pivot takes an integer argument between 1 and 5 that tells it
which reinterpretation to make

Here's the really interesting thing. Any major or minor triad could be seen as 
the 1, 3, 4, 5, or 6 chord of some key, while majors can also be 7s and minors
can also be 2s. 

-}


pivotChord :: MChord -> Int -> MChord
pivotChord (nte, dur, patt, typ, (rt, mode, octv)) n =
    if n == nte then (nte, dur, patt, typ, (rt, mode, octv)) else
    if nte == 1 && mode == "Min" then
        (6, dur, patt, typ, (mod (rt + 3) 12, "Maj", octv))
    else if nte == 6 && mode == "Maj" then
        (1, dur, patt, typ, (mod (rt + 9) 12, "Min", octv))
    else        
     let absNte = toAbsNote nte mode
         (desNte, newMode) = if (mode == "Maj" && elem nte [0,3,4]) || 
                                (mode == "Min" && elem nte [2,5,6]) then
                               [(0, "Maj"),(10, "Min"),(3, "Min"),(5, "Maj"),
                                (7, "Maj"),(8, "Min")]!!n
                            else [(0, "Min"),(2, "Maj"),(4, "Maj"),(5, "Min"),
                                  (7, "Min"),(9, "Maj")]!!n
         nteDiff' = absNte - desNte
         nteDiff = if nteDiff' < 0 then 12 + nteDiff' else nteDiff'
         newRt = mod (rt + nteDiff) 12
         newNte = if newMode == "Min" && n == 2 then 6 else n
     in (n, dur, patt, typ, (newRt, newMode, octv))
  where
    toAbsNote nte mode =
        case mode of 
            "Maj" -> [0,2,4,5,7,9,11]!!nte
            "Min" -> [0,2,3,5,7,8,10]!!nte
            otherwise -> toAbsNote nte "Maj"
            
moveChord :: MChord -> Int -> MChord
moveChord (nte, dur, patt, typ, key) n = 
    (mod (nte+n) 7, dur, patt, typ, key)
    
--we can apply some heuristics to make smooth moves between chords by 
--changing the type. More heuristics can be added later
    
--Note: only use moveSmooth with root, six, and six-four chords! Other chords
--will freak it out    
    
moveSmooth :: MChord -> Int -> MChord
moveSmooth c@(nt,_,_,_,_) n =
    let (newNt, dur, patt, typ, key) = moveChord c n
    in if newNt == mod (nt + 5) 7 then (newNt, dur, patt, cyc 0 typ, key)
    else if newNt == mod (nt + 2) 7 then (newNt, dur, patt, cyc 1 typ, key) 
    else (newNt, dur, patt, typ, key)
  where
    cyc k typ = 
        case typ of 
            "root" -> ["six", "six-four"]!!k
            "six" -> ["six-four", "root"]!!k
            "six-four" -> ["root", "six"]!!k
            
cCadence :: MChord -> (Rational, Rational, Rational) -> [MChord]

cCadence (_,d,p,_,(rt,md,oct)) (d1, d2, d3) = 
    [(3,d*d1,p,"root",(rt,md,oct)),
     (4,d*d2,p,"root",(rt,"Maj",oct)),
     (0,d*d3,p,"root",(rt,md,oct))]
    
scaleDur :: MChord -> Rational -> MChord
scaleDur (n,d,p,t,k) s = (n,d*s,p,t,k)

switchMode :: MChord -> MChord
switchMode (n,d,p,t,(rt,md,o)) = 
    if md == "Maj" then (n,d,p,t,(rt,"Min",o)) else (n,d,p,t,(rt,"Maj",o))
    
switchDur :: MChord -> Int -> MChord
switchDur (n,d,p,t,k) x = (n,d,["p1","p2","p3"]!!x,t,k)

----------------------------Bass Part ------------------------------------------

--the bass line is formed from the MChord list along with the transformation list

--the first thing we need is a map from transformations to bass movement

--a BMusic will be a (note, duration, octave) tuple

type BMusic = (Int, Rational, Int)

tToBassMove :: [Int] -> [String]
tToBassMove [] = [] 
tToBassMove (n1:ns) =
    ctsToBass (ctList1!!n1) 0 : tToBassMove ns
  where 
    ctsToBass [("Cadence", _)] _ = "Cadence"
    ctsToBass [] sum = if sum < 0 then "Down"  
                       else if sum > 0 then "Up"
                       else "Stay" 
    ctsToBass ((s, c):cs) sum = if s == "Move" || s == "sMove" then
                                 ctsToBass cs (sum + c)
                                else ctsToBass cs sum

--then we combine the MChord list with the move list

tToBass :: [Int] -> [BMusic]
tToBass ns = 
    let mChords = numsToChordList ns
        mMove = "Stay" : tToBassMove ns
        mapChord [] _ _ = []
        mapChord _ [] _ = []
        mapChord ((nt,dur,_,_,(rt,mde,_)):cs) (m1:ms) (lst, _, oct) =
            let bNteChng = case mde of 
                            "Maj" -> [0,2,4,5,7,9,11]!!nt
                            "Min" -> [0,2,3,5,7,8,10]!!nt
                newRt = mod (rt + bNteChng) 12
                newOct = if newRt < lst && m1 == "Up" && oct < 4 then oct + 1
                         else if newRt > lst && m1 == "Down" && oct > 2 then oct - 1
                         else oct
                newB = (newRt, dur, newOct)
            in newB : mapChord cs ms newB
        (rtInit, durInit) = (\(_,dIn,_,_,(rIn,_,_)) -> (rIn, dIn)) cInitial
    in mapChord mChords mMove (rtInit, durInit, 2)
    
bMusToMPitch :: [BMusic] -> Music Pitch
bMusToMPitch [] = rest 0
bMusToMPitch ((nt,dur,oct):bs) = 
    fst ((notesN oct)!!nt) oct dur :+: bMusToMPitch bs
    
numsToBass :: [Int] -> Music Pitch
numsToBass = bMusToMPitch . tToBass    
    
--now we can play chords and bass, with bass motion hopefully making the 
--transformations more clear

playCandB :: [Int] -> Rational -> IO ()
playCandB numList t = play $ tempo (t/120) (instrument RhodesPiano (numsToChords numList) 
                        :=: instrument Piccolo (numsToBass numList))
                        
                        
--Peter Lewis, 2012
                          

        


    
    

 
            



















    