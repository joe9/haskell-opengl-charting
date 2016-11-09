{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE PackageImports #-}

module Drawable where

--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageImportsProposal
import Protolude
import Data.Colour
import Control.Monad
import Control.Monad.Trans.Cont
import Data.IORef
import qualified Data.Vector.Unboxed as VU
import "gl" Graphics.GL
import Graphics.UI.GLFW as GLFW
import Prelude hiding (init)
import Data.Int
--
import Scale
import Types
import GLFWHelpers
import OpenGLHelpers

data Value
    = ValueCursorPosition Double
                          Double
    | ValueDimensions Width
                      Height
    | ValueInt Int
    | ValueAsOf Int64
    | ValueInteger Integer
    | ValueEmpty
    deriving (Eq)

data DrawableType
    = Screen
    | Frame
    | PriceChart
    | VolumeChart
    | HorizontalCrosshair
    | VerticalCrosshair
    deriving (Show)

data Drawable = Drawable
    { dPreviousValue :: Maybe Value
    , dCurrentValue :: State -> VU.Vector PriceData -> Value
    , dLoadBufferAndBuildDrawFunction :: State -> VU.Vector PriceData -> Scale -> Scale -> Scale -> Drawable -> IO (IO ())
    , dDraw :: IO ()
    , dVertexArrayId :: VertexArrayId
    , dBufferId :: BufferId
    , dColour :: Colour Double
    , dTransparency :: Maybe Double
    , dType :: DrawableType
    }

renderDrawables
    :: Env
    -> State
    -> [Drawable]
    -> (VU.Vector PriceData, Scale, Scale, Scale)
    -> IO [Drawable]
renderDrawables env state ds dataSeries@(series,_,_,_) = do
    let win = envWindow env
        colorUniformLocation = envColorUniformLocation env
    putStrLn "renderDrawables called"
    if (any
            (\d ->
                  Just (dCurrentValue d state series) /= dPreviousValue d)
            ds)
        then do
            newds <-
                mapM (\d -> renderDrawable win colorUniformLocation state d dataSeries) ds
            GLFW.swapBuffers win
            glFlush  -- not necessary, but someone recommended it
            return newds
        else return ds

renderDrawable
    :: Window
    -> ColorUniformLocation
    -> State
    -> Drawable
    -> (VU.Vector PriceData, Scale, Scale, Scale)
    -> IO Drawable
renderDrawable win colorUniformLocation state drawable (series,xscale,pricescale,volumescale) = do
    let justDraw =
            (\d -> do
                 drawUsingVertexArray
                     win
                     colorUniformLocation
                     (dVertexArrayId d)
                     (dColour d)
                     (dTransparency d)
                     (dDraw d)
                 return d)
    let newValue = dCurrentValue drawable state series
    if (Just newValue /= dPreviousValue drawable)
        then do
            putStrLn
                ("renderDrawable called - loading buffer and drawing of " ++
                 show (dType drawable))
            -- With OpenGL, the coordinates should be in the range (-1, 1)
            drawFunction <-
                (dLoadBufferAndBuildDrawFunction drawable)
                    state
                    series
                    xscale
                    pricescale
                    volumescale
                    drawable
            justDraw
                (drawable
                 { dDraw = drawFunction
                 , dPreviousValue = Just newValue
                 })
        else do
            putStrLn
                ("renderDrawable called - drawing " ++ show (dType drawable))
            justDraw drawable

-- could use the ContT monad. but, this is more readable
--  https://github.com/glguy/irc-core/blob/v2/src/Client/CApi.hs#L146-L158
-- initializeDrawables
--     :: ([Drawable] -> IO b) -> IO b
-- initializeDrawables continueFunction =
--     withVertexArray $
--     \svaid svabid -> do
--         withVertexArray $
--             \fvaid fvabid -> do
--                 withVertexArray $
--                     \pvaid pvabid -> do
--                         withVertexArray $
--                             \vvaid vvabid -> do
--                                 withVertexArray $
--                                     \hcvaid hcvabid -> do
--                                         withVertexArray $
--                                             \vcvaid vcvabid -> do
--                                                 continueFunction
--                                                     [ screenDrawable
--                                                           svaid
--                                                           svabid
--                                                     , frameDrawable
--                                                           fvaid
--                                                           fvabid
--                                                     , priceChartDrawable
--                                                           pvaid
--                                                           pvabid
--                                                     , volumeChartDrawable
--                                                           vvaid
--                                                           vvabid
--                                                     , horizontalCrosshairDrawable
--                                                           hcvaid
--                                                           hcvabid
--                                                     , verticalCrosshairDrawable
--                                                           vcvaid
--                                                           vcvabid]


-- the above can be refactored to
--   https://wiki.haskell.org/MonadCont_under_the_hood
-- continuation takes a function and applies it within it's context
-- Cont b a: takes a function (a -> b) and returns b. It has a in it's belly.
-- ContT b m a: continuation transformer
-- replicateM replicates an object: basically, m x -> m [x]
-- replicateM 2 (ContT b m a) => ContT b m [a]
-- replicateM applies the >>= of the current monad when figuring out
--    how to compute the [x] of m [x]
-- when using replicateM, the continuation function needed has changed
--    from (a -> b) to ([a] -> b)
-- runCont is how the continuation is completed. i.e., take the
--    function needed for the Cont b a, run Cont b a with the function
--    (a -> b) as the argument and and return the b

-- <ski> joe9 : the point is to execute multiple `ContT withBlah' actions in a row
-- <joe9> ski, but, how does chaining continuations work with replicateM work? replicateM just duplicates objects (in this case, continuation objects). T
-- <joe9> ski, oh, Ok. Thanks.
-- <heebo> sorry to be clear, im shocked and disappointed at macos sierra update that breaks stack  [12:50]
-- <heebo> anyone else experiencing this?
-- <ski> joe9 : like `do x <- ContT withBlahX; y <- ContT withBlahY; z <- ContT withBlahZ; return (x,y,z)', instead of `withBlahX $ \x -> withBlahY $ \y -> withBlahZ $ \z ->'
-- <ski> joe9 : and the former can be abbreviated as `do [x,y,z] <- mapM ContT [withBlahX,withBlahY,withBlahZ]; return (x,y,z)'  [12:51]
-- <ski> joe9 : in case `withBlahX',`withBlahY',`withBlahZ' are all the same thing, say `withBlah', then you have `mapM ContT [withBlah,withBlah,withBlah]', which is `mapM ContT
--       (replicate 3 withBlah)', which is `replicateM 3 (ContT withBlah)'  [12:53]
-- <joe9> ski, shouldn't it be: `let f = ContT withBlahX >>= ContT withBlahY >>= ContT withBlahZ '
-- <joe9> ski, oh, and replicateM does that sequencing.  [12:55]
-- <ski> joe9 : we want to collect the `x',`y',`z' results from the `withBlahX',`withBlahY',`withBlahZ' calls (assuming they have the same return type, otherwise we'd use `liftM3
--       (,,) (ContT withBlahX) (ContT withBlahY) (ContT withBlahZ)' instead of `mapM ContT [withBlahX,withBlahY,withBlahZ]')
-- <ski> yes
-- <ski> @src replicateM
-- <lambdabot> replicateM n x = sequence (replicate n x)
-- <ski> @src mapM
-- <lambdabot> mapM f as = sequence (map f as)
-- <ski> your `ContT withBlahX >>= ContT withBlahY >>= ContT withBlahZ' would try to feed `x' directly onwards to the next part  [12:56]
-- <joe9> ski, that is where I am getting stuck "feed 'x' directly onwards to the next part"  [12:58]
-- <ski> (the `return' above is btw just to not drop `x',`y',`z' on the floor. you could have something more involved there. but the point is that you *can* just `return' them in
--       the `ContT b IO' monad, and still have whatever followes the call of this `f' intuitively be "nested inside" it)
-- <ski>   ContT withBlahX >>= ContT withBlahY >>= ContT withBlahZ  [12:59]
-- <ski> means
-- <ski>   (ContT withBlahX >>= ContT withBlahY) >>= ContT withBlahZ
-- <ski> means
-- <ski>   (ContT withBlahX >>= \x -> ContT withBlahY x) >>= \y -> ContT withBlahZ y
-- <joe9> ski, i think i understand this part. i was getting tripped on how the replicateM does the sequencing or the >>=  [13:00]
-- <ski> but here, that doesn't make sense
-- <ski> `ContT withBlahY' is not a function, nor is `ContT withBlahZ' one
-- <ski> well, consider  [13:01]
-- <ski>   do x <- ContT withBlahX; y <- ContT withBlahY; z <- ContT withBlahZ; return (x,y,z)
-- <ski>   do [x,y,z] <- sequence [ContT withBlahX,ContT withBlahY,ContT withBlahZ]; return (x,y,z)
-- <ski>   do [x,y,z] <- sequence (map ContT [withBlahX,withBlahY,withBlahZ]); return (x,y,z)  [13:02]
-- <ski>   do [x,y,z] <- mapM ContT [withBlahX,withBlahY,withBlahZ]; return (x,y,z)
-- <ski> now, assume that we use `withBlah' for `withBlahX',`withBlahY',`withBlahZ' :
-- <ski>   do [x,y,z] <- mapM ContT [withBlah,withBlah,withBlah]; return (x,y,z)  [13:03]
-- <ski>   do [x,y,z] <- mapM ContT (replicate 3 withBlah); return (x,y,z)
-- <ski>   do [x,y,z] <- sequence (map ContT (replicate 3 withBlah)); return (x,y,z)
-- <joe9> I get ContT r IO [x]
-- <ski>   do [x,y,z] <- sequence (replicate 3 (ContT withBlah)); return (x,y,z)
-- <ski>   do [x,y,z] <- replicateM 3 (ContT withBlah); return (x,y,z)
-- <ski> this is using  [13:04]
-- <ski> @src mapM
-- <lambdabot> mapM f as = sequence (map f as)
-- <ski> @src replicateM
-- <lambdabot> replicateM n x = sequence (replicate n x)
-- <ski> and the intuitive meaning of `sequence' to collect a sequence of actions, where neither of the latter depends on the monadic results of the former (which is the case here)
-- <joe9> ski, I get it. Thanks a lot.
-- <joe9> ski, got it, man. Thanks for being so patient.  [13:05]
-- <nitrix> @pl (\s -> print s >> threadDelay 1000000)
-- <lambdabot> (>> threadDelay 1000000) . print
-- <ski> and also depends on `replicate' being a natural transformation, so that we know `map f . replicate n = replicate n . f', for any `f' and `n'
-- <joe9> ski, it is just m x -> m [x]
-- <ski> joe9 : it takes some while to get used to how `Cont'/`ContT' works with stuff like this, but imho, it can be well worth it  [13:06]
-- <hodapp> I am really, really at a loss for why cabal is complaining about a file in a completely different build that I haven't used for a year and that is in no way related to
--          the thing I'm trying to build.
-- <ski> joe9 : np
-- <nitrix> :t (>=>)
-- <lambdabot> Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- <joe9> ski, ok, Thanks. I worked through the ContT stuff a few years ago. and totally forgot about it now.
-- <nitrix> Is there a...  [13:07]
-- <nitrix> Monad m => (a -> m b) -> m c -> a -> m c  ?
-- <ski> not that i recall  [13:08]
-- <nitrix> >=> that's closer to >> disregarding the argument ?
-- <ski> add a `const', i suppose
-- <ski> @type \amb mc -> amb >=> const mc
-- <lambdabot> Monad m => (a -> m b) -> m c -> a -> m c  [13:09]
-- * ski is always happy to see someone using continuations :)  [13:10]
-- <nitrix> I shall create >.>
-- <hodapp> ski: just writing up a post now at HaskellEmbedded about how I learned to stop worrying and love continuations (for embedded programming)
-- <hodapp> or something like that  [13:11]
-- <jle`> i wish there was a way to get a continuation monad to work with skolemized existentials
-- <jle`> there might be a way with rebindablesyntax maybe :|
-- <ski> nitrix : not `>=.' ?  [13:12]
-- <ski> (to emphasize which side isn't a kleisli arrow)
-- <nitrix> That's theorically what it should be.
-- <nitrix> But that doesn't make a cute face.
-- <nitrix> :P  [13:13]
-- * ski shrugs
-- <EvanR> face oriented programming
-- <nitrix> Programming With Emojis In Haskell
-- <EvanR> exists
-- <nitrix> I hope that never gets quoted anywhere with my name on it.
-- <EvanR> https://twitter.com/emojihaskell  [13:14]
-- <dagda1> can anyone explain why my isOdd function is not returning the results I expect https://gist.github.com/dagda1/5def666d482ec5d0751a641eef2d5564  [13:22]
-- <joe9> ski, http://bpaste.net/show/1c73467042ca if you do not mind, can you skim through this to ensure that my understanding is correct.
-- <joe9> ski, sorry for bothering you. I just want to make sure that I get it right.
-- <ski> joe9 : sounds mostly right  [13:24]
-- <joe9> ski, Thanks a lot.
-- <ski> joe9 : what is passed to `runCont'/`runContT' will be placed innermost in the nesting  [13:25]
-- <ski> (not sure how well you grasped that)
-- <joe9> ski, yes, I get that. It will the innermost basically nested down.  [13:26]
-- <joe9> ski, that is how the >>= of the Cont monad is defined.
-- <joe9> ski, take the next function and apply it within my context ( or, belly)
-- <ski> joe9 : with `replicateM 2 (... :: ContT b m a)', you get something of type `ContT b m [a]', and so when using `runContT', you should provide a continuation of type `[a] ->
--       m b', not `a -> b' as you said
-- <joe9> ski, runCont is just unwrapping. It is the sequence or the >>= that does the magic of applying the next function within my context.  [13:27]
-- * hackagebot ion 1.0.0.0 - EDSL for concurrent, realtime, embedded programming on top of Ivory  https://hackage.haskell.org/package/ion-1.0.0.0 (hodapp)
-- * hackagebot data-msgpack 0.0.4 - A Haskell implementation of MessagePack  https://hackage.haskell.org/package/data-msgpack-0.0.4 (iphydf)
-- <joe9> ski, oh, gotcha. interesting. good catch. let me think that through.
-- <joe9> ski, i totally misunderstood that.  [13:29]
-- <joe9> ski, I think I get it now.
-- <hodapp> woo! hackagebot acknowledged something I did!  [13:31]
-- <dedgrant> dagda1: What is the result of (elemIndex 18 [18,18])?  (May help illustrate the error in the strategy.)  [13:33]
-- <dagda1> dedgrant: ha of course
-- <bjorn__> how to integrate a haskell script inside a stack project?
-- <bjorn__> where should i put it in the cabal file  [13:34]
-- <dagda1> @dedgrant is it possible to get the index with foldl?
-- <lambdabot> Unknown command, try @list
-- <jonored> dagda1: zip will get you there, at least.  [13:35]
-- <dagda1> @jonored great, thanks  [13:36]
-- <lambdabot> Unknown command, try @list
-- <jonored> fold someFunc (zip [1..] stuff)
-- <joe9> ski, this is what replicateM is doing :t k >>= (\x -> k >>= \y -> return [x,y]) >>= (\(x:y:[]) -> k >>= \z -> return [x,y,z])
-- <dedgrant> dagda1: Certainly. You can zip up pairs of [(Index,Value)] and filter on that.  I suspect whatever exercise source this comes from may be expecting a solution to be
--            written as pattern-matched recursion.
-- <joe9> ski, :t k >>= (\x -> k >>= \y -> return [x,y]) >>= (\xy -> k >>= \z -> return (xy ++ [z]))  [13:37]
-- <joe9> k >>= (\x -> k >>= \y -> return [x,y]) >>= (\xy -> k >>= \z -> return (xy ++ [z]))
-- <joe9>   :: ContT r IO [(VertexArrayId, BufferId)]
-- <joe9> ski, to understand this: k >>= -- this >>= is the Cont monad's >>=  [13:38]
-- <joe9>  ski, (\x -> k >>= \y -> return [x,y]) -- the >>= here is the IO monad's >>=?  [13:39]
-- <joe9> ski, this (\x -> k >>= \y -> return [x,y])  is the replicateM plumbing, correct?  [13:41]
-- <ski> joe9 : "runCont is just unwrapping", sure :) doesn't change what i said, though :)  [13:42]
-- <ski> <joe9> ski, this is what replicateM is doing :t k >>= (\x -> k >>= \y -> return [x,y]) >>= (\(x:y:[]) -> k >>= \z -> return [x,y,z])  [13:43]
-- <ski> where does `k' come from ?
-- <joe9> ski: let k = (ContT (withVertexArray . curry))
-- <joe9> :t k :: ContT r IO (VertexArrayId, BufferId)
-- <lambdabot> error:
-- <lambdabot>     Not in scope: type constructor or class  VertexArrayId
-- <lambdabot> error:
-- <joe9> ski ^^  [13:44]
-- <ski> well, `replicateM' doesn't repeatedly pack and unpack the list like that
-- * ski renames `k' to `with'  [13:45]
-- <ski> what it's doing is more like
-- <ski>   \k -> act >>= \x -> act >>= \y -> act >>= \z -> k [x,y,z]  [13:46]
-- <ski> where `k' here is what you pass to `runContT', when you decide to get out of `ContT'
-- <joe9> ok, got it. Thanks.
-- <ski> (the result is the same, of course. just pointing out that it's not repeatedly adding an element to the end of an accumulating list)  [13:47]
-- <ski> (er, apparently i renamed it to `act' ..)
-- <joe9> ski, sorry for the bother. The sequence is one doing the actual foldr or nesting when it applies the >>= of the continuation monad, correct?  [14:09]
-- <joe9> ski, http://hackage.haskell.org/package/base-4.9.0.0/docs/src/Data.Traversable.html#sequenceA the traverse function for list is doing the foldr and accumulating the
--        results  [14:10]
-- <joe9> ski, better link http://hackage.haskell.org/package/base-4.9.0.0/docs/src/Data.Traversable.html#traverse
-- <joe9> ski     traverse f = List.foldr cons_f (pure []) where cons_f x ys = (:) <$> f x <*> ys  [14:11]
-- <joe9> ski, the ".. <$> f x ..", the f is the continuation bind function, correct?  [14:14]
-- <ski> joe9 : can't quite parse "The sequence is one doing the actual foldr or nesting"  [14:24]
-- <ski> joe9 : "the f is the continuation bind function, correct?" -- no, `f' there is the "body" callback argument to `traverse', which is similar to `mapM'  [14:27]
-- * hackagebot network-msgpack-rpc 0.0.2 - A MessagePack-RPC Implementation  https://hackage.haskell.org/package/network-msgpack-rpc-0.0.2 (iphydf)
-- <joe9> ski, ok. Thanks a lot.  [14:31]
-- <ski> the continuation passed for the `ContT' version would only be visible if you expand `(>>=)' (or `(<*>)', in case of `traverse')  [14:32]
-- <ski> (i assume that by "continuation bind function" you meant that continuation .. sometimes the right argument to `(>>=)' is also referred to as a continuation, though ..)

-- the above can be refactored to
-- <ski>   replicateM 6 (ContT (withVertexArray . curry)) `runContT` \[(svaid,svabid),(fvaid,fvabid),(pvaid,pvabid),(vvaid,vvabid),(hcvaid,hcvabid),(vcvaid,vcvabid)] ->
--       continueFunction [..as before..]  [10:59]
-- <ski> alternatively
-- <ski>   replicateM 6 (ContT (withVertexArray . curry)) `runContT` (continueFunction . zipWith uncurry
--       [screenDrawable,frameDrawable,priceChartDrawable,volumeChartDrawable,horizontalCrosshairDrawable,verticalCrosshairDrawable])
withInitializedDrawables :: [(VertexArrayId -> BufferId -> Drawable)] -> ([Drawable] -> IO b) ->  IO b
withInitializedDrawables dfs continueFunction =
  runContT (replicateM (length dfs) (ContT (withVertexArray . curry)))
  -- the above ContT b IO [(VertexArrayId,BufferId)] needs a
  --   function of type [(VertexArrayId,BufferId)] -> IO b to complete
           (continueFunction . zipWith uncurry dfs)

