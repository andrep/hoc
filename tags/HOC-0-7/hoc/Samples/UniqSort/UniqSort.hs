module Main where

import Foundation
import Foundation.NSProcessInfo (arguments)
import HOC

-- int main (int argc, const char * argv[]) {
main = do
--   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
--   ...
--   [pool release];
     withAutoreleasePool main'

main' = do
--   NSArray *args = [[[NSProcessInfo processInfo] arguments];
     args <- _NSProcessInfo # processInfo >>= arguments
--   NSCountedSet *cset = [[NSCountedSet alloc] initWithArray:args];
     cset <- _NSCountedSet # alloc >>= initWithArray args
--   NSArray *sorted_args = [[cset allObjects]
--     sortedArrayUsingSelector:@selector(compare:)];
     sorted_args <- cset # allObjects
       >>= sortedArrayUsingSelector (getSelectorForName "compare:")
--   NSEnumerator *enm = [sorted_args objectEnumerator];
     enm <- sorted_args # objectEnumerator
--   id word;
--   while (word = [enm nextObject]) {
--     printf("%s\n", [word UTF8String]);
--   }
     let loop = do
	 word <- enm # nextObject
	 if (not (isNil word))
	   then do { putStrLn ( (fromNSString.castObject) word ); loop }
	   else do return ()
         in
             loop
--   [cset release];
     -- Don't need to release anything: HOC manages memory for you via
     -- Haskell's garbage collector!
--   return 0;
     return ()
-- }


{-
   Note that an alternative Haskell implementation would have been:

     main = do
       words <- System.getArgs
       mapM_ putStrLn ( (sort.nub) words )

   ... which is a bit shorter and neater. :)
-}

