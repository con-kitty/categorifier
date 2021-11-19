{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Various tools for improving the quality of the failures in your programs.  This is meant to
-- encourage you to "do the right thing" more often.
--
-- == Usage
--
-- __Please import this module instead of "Control.Exception"__, and __please import it qualified__,
-- e.g. as @Exception@.
--
-- See
-- [the Exceptions wiki page](https://artifacts.heavisoft.kittyhawk.aero/refs/heads/main/bazel-bin/doc/haddock/haddock/commonZSioZSexception/Kitty-IO-Exception.html) [ ](DONTLINTLINELENGTH)
-- for a more detailed discussion of why and how to deal with exceptions and internal errors in your
-- programs.
--
-- == Problems with built-in exception handling solved by this module
--
-- === GHC does not provide call stack info by default
--
-- This is problematic because programmers must use the `HasCallStack` constraint and `callStack`
-- function to manually include call stack information with any exceptions they report.  This module
-- makes this process easier with `CallStacked` and associated functions and provides call stack
-- info automatically in its main functions.
--
-- === GHC requires `Show` instances for all exception types
--
-- This is problematic because `Show` is meant to provide a representation of the Haskell source
-- corresponding to your value, not the user-friendly message for which GHC's
-- `Control.Exception.Exception` class abuses it (and which a library function is in general
-- ill-equipped to provide).  This module solves this problem with `AsException` and associated
-- functions, which take a rendering function provided where the exception is thrown and which can
-- be replaced by functions closer to @main@ (as opposed to a type class method) which may be able
-- to provide more useful information to users.
--
-- GHC's `Control.Exception.Exception` class actually provides a way around the coupling of `Show`
-- with exception reporting via the `Control.Exception.displayException` method of the
-- `Control.Exception.Exception` class, but:
--
-- === GHC uses `show` rather than `Control.Exception.displayException` to display exceptions
--
-- This is problematic for the reasons mentioned above.  This module solves this problem with
-- `Display` and associated functions, which change the behavior of exception-printing.
--
-- See the StackOverflow thread
-- [Why doesn't GHC use my `displayException` method?](https://stackoverflow.com/questions/55490766/why-doesn-t-ghc-use-my-displayexception-method)  [ ](DONTLINTLINELENGTH)
-- for some explanation. I find it totally unconvincing, and I think Kmett's comment about
-- "\'helpful\' `Show` instances" makes the argument /for/ using `displayException` -- with the
-- current behavior, users are encouraged to define a custom `show` to get GHC to output a useful
-- failure message, which then breaks the /intended/ use of `show` as an syntax printer.
--
-- === Asynchronous exceptions are tricky
--
-- In general it's quite challenging to write side-effecting code that does the right thing in the
-- face of Haskell's asynchronous exceptions.  There are a lot of subtle gotchas that result in
-- programs mysteriously dying or failing to clean up resources.  This module solves this problem by
-- delegating everything about it to the @unliftio@ package, which is designed specifically to solve
-- this problem.
--
-- == Provenance and compatibility
--
-- This module is derived directly from
-- [Greg Pfeil's @beautiful-failures@ code](https://github.com/sellout/beautiful-failures). [ ](DONTLINTLINELENGTH)
-- This module also based on the lifted versions of things like `Exception.throwIO` from the
-- [@unliftio@](https://hackage.haskell.org/package/unliftio) package.  It's fully compatible with
-- everything in @unliftio@.
module Kitty.Common.IO.Exception
  ( -- * I need to report an error; just get me started.

    -- | Correct usage of this module often boils down to the following:
    --
    --     1. Define a custom type @MyError@ describing the errors your function or system can
    --        produce
    --
    --     2. @import qualified "Kitty.Common.IO.Exception" as Exception@ wherever you need to work
    --        with exceptions
    --
    --     3. Call @`Exception.throwIOAsExceptionWithCallStack` render err@ in your program where
    --        you have encountered an exceptional condition.  (Here @err@ has the type @MyError@
    --        from the first step.)
    --
    --     4. @main = Exception.displayExceptions $ do ...@ for your main function
    --
    -- This should do the right thing both for users of your program (provides useful error
    -- messages) and people reusing your code (provides structured error information to work with).
    throwIOAsExceptionWithCallStack,
    displayExceptions,
    throwIOAsExceptionWithCallStackLeft,

    -- ** Providing more accurate call stack information
    addCallStack,

    -- ** Reporting programmer errors or violated invariants from pure code
    throwAsExceptionWithCallStack,

    -- * Fine-grained components for correct error handling

    -- | This module is essentially a mix of @beautiful-failures@ tricks and @unliftio@.

    -- ** Attaching a `CallStack` to exceptions
    CallStacked (..),
    catchIgnoringStack,
    handleIgnoringStack,
    throwIOWithCallStack,

    -- ** Treating arbitrary types as exceptions
    AsException (..),
    throwIOAsException,
    throwAsException,

    -- ** Make exceptions useful as an exit mechanism
    Display (..),
    catchIgnoringDisplay,
    handleIgnoringDisplay,

    -- ** Consuming disjunctions
    throwIOLeft,

    -- ** For plugin use
    impureThrow,

    -- * Re-exports from "UnliftIO.Exception"
    Exception.Exception (..),
    Exception.SomeException (..),
    Exception.throwIO,

    -- ** Exception management
    Exception.catch,
    Exception.handle,
    Exception.mask,
    Exception.try,
    Exception.evaluate,

    -- ** Exception management with normal-form evaluation
    Exception.catchDeep,
    Exception.handleDeep,
    Exception.tryDeep,
    Exception.evaluateDeep,

    -- ** Cleanup in the face of asynchronous exceptions
    Exception.bracket,
    Exception.bracket_,
    Exception.finally,
  )
where

import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Typeable (Typeable)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (Exception (..), SomeException)
import qualified UnliftIO.Exception as Exception

-- | This is a trivial wrapper, but it prevents `impureThrow` from being inlined too soon for the
--  "Kitty.Cat" plugin to handle it. It also doesn't work using `Exception.impureThrow` instead of
--  `throw` (presumably becasue of the `Exception.SyncExceptionWrapper` handling).
impureThrow :: Exception e => e -> a
impureThrow = throw
{-# NOINLINE impureThrow #-}

-- | An Exception that simply wraps another exception to attach a call stack
data CallStacked e = CallStacked {callStackException :: e, callStackCalls :: CallStack}

-- | See `handleIgnoringStack`.
catchIgnoringStack :: (MonadUnliftIO m, Exception e) => m a -> (e -> m a) -> m a
catchIgnoringStack = flip handleIgnoringStack

-- | Like `Exception.handle`, but checks for both the "bare" and `CallStacked` versions of an
-- exception.
--
--  __NB__: This does mean that if you re-`Exception.throwIO` you'll discard the original
--         `CallStack`.
handleIgnoringStack :: (MonadUnliftIO m, Exception e) => (e -> m a) -> m a -> m a
handleIgnoringStack f = Exception.handle f . Exception.handle (\(CallStacked e _) -> f e)

-- | See the instance for @`Show` (`AsException` e)@ to understand the job this instance does.
instance Exception e => Show (CallStacked e) where
  show = displayException

instance Exception e => Exception (CallStacked e) where
  displayException (CallStacked e calls') = displayException e <> "\n" <> prettyCallStack calls'

-- | Bundles up the current call stack information with the given value.
addCallStack :: HasCallStack => e -> CallStacked e
addCallStack e = CallStacked e callStack

-- | A way to lift reified errors into the exception system. This means we get the best of both
--   worlds when we're in IO - we can fail with a locally-defined message and still leave it up to
--   someone to catch it if necessary.
--
--   Without this, being able to `Exception.throw` your structured errors means defining a `Show`
--   instance (that you might not otherwise want) right next to the error definition rather than
--   close to the user.
--
--   Since this is likely to be used far from where an error occurs, it doesn't include a
--   `CallStack`. But `CallStacked` is available if you want to add a stack to it.
data AsException e = AsException
  { displayAsException :: e -> String,
    getException :: e
  }

-- | __NB__: This is not a proper `Show` instance.
--
-- GHC uses `show` rather than `displayException` to display exceptions that occur at the top level.
-- The point of `AsException` is to allow you to signal exceptional conditions in a type-safe way
-- without requiring `Show` or `Exception` instances, and it already requires you to provide a
-- helpful way of displaying it.  This instance exists only to get around GHC's behavior and connect
-- top-level display of exceptions to the `displayAsException` field of your `AsException`.
instance Typeable e => Show (AsException e) where
  show = displayException

instance Typeable e => Exception (AsException e) where
  displayException (AsException display failure) = display failure

-- | Throws any type. The provided function is what's used for serialization if the exception isn't
--   caught.
throwIOAsException :: (MonadIO m, Typeable e) => (e -> String) -> e -> m a
throwIOAsException f = Exception.throwIO . AsException f

throwAsException :: (Typeable e) => (e -> String) -> e -> a
throwAsException f = Exception.impureThrow . AsException f

-- | Throws an exception that's expecting to carry a `CallStack`.
--
--   I /think/ these two should be equivalent
--
-- > throwIOWithCallStack . CallStacked
-- > throwIO . addCallStack
--
-- See `throwIOAsExceptionWithCallStack` for an __important note about call stacks__ resulting from
-- this function.  The note is attached to that function instead of this one since it is used more
-- often.
throwIOWithCallStack :: (HasCallStack, MonadIO m, Exception e) => (CallStack -> e) -> m a
throwIOWithCallStack = Exception.throwIO . ($ callStack)

-- | You give this function some error value and a way to display it as an exception message (in
-- case nobody catches it), and it appends the /current/ call stack and throws your exception.
-- (This is just a blend of `throwIOAsException` and `throwIOWithCallStack` for your convenience.)
--
-- __Important note about call stacks__: This function provides the `CallStack` itself.  This means
-- whoever receives your thrown exception will see a call stack leading to where you have called
-- `throwIOAsExceptionWithCallStack`, __not leading to where the failure originally occurred__.
-- Before calling this function, consider whether the corresponding failure takes place far enough
-- from where you will throw the exception that it makes sense to form your own stack trace with
-- e.g. `addCallStack` or @`flip` `CallStacked` `callStack`@ where the failure occurs and throw it
-- directly using `Exception.throwIOAsException`.
throwIOAsExceptionWithCallStack ::
  (HasCallStack, MonadIO m, Typeable e) =>
  (e -> String) ->
  e ->
  m a
throwIOAsExceptionWithCallStack f =
  Exception.throwIO . flip CallStacked callStack . AsException f

-- | Eliminate an @`Either` e a@ by throwing any @`Left` e@ values encountered, appending call stack
-- info and not requiring an `Exception` instance for the error type @e@.
--
-- > throwIOAsExceptionWithCallStackLeft render =
-- >   either (throwIOAsExceptionWithCallStack render) pure
throwIOAsExceptionWithCallStackLeft ::
  (HasCallStack, MonadIO m, Typeable e) =>
  (e -> String) ->
  Either e a ->
  m a
throwIOAsExceptionWithCallStackLeft f =
  either (throwIOAsExceptionWithCallStack f) pure

-- | This is a "pure" version of `throwIOAsExceptionWithCallStack`; the result may be anything, not
-- just a value in `MonadIO`.  This is not great, but it's better than `Prelude.error`.
throwAsExceptionWithCallStack ::
  (HasCallStack, Typeable e) =>
  (e -> String) ->
  e ->
  a
throwAsExceptionWithCallStack f =
  impureThrow . flip CallStacked callStack . AsException f

-- | Eliminate an `Either` by throwing the left.
--
-- > throwIOLeft = either Exception.throwIO pure
throwIOLeft :: (MonadIO m, Exception e) => Either e a -> m a
throwIOLeft = either Exception.throwIO pure

-- | A wrapper to convince GHC to use `displayException` when printing failures.
newtype Display a = Display a

-- | See the instance for @`Show` (`AsException` e)@ to understand the job this instance does.
instance Exception e => Show (Display e) where
  show (Display e) = displayException e

instance Exception e => Exception (Display e)

-- | This "fixes" @main@ to use `displayException` instead of `show` when failing with an exception.
--
-- E.g., replace @main = x@ with @main = displayExceptions $ x@. This is also useful before
-- `System.IO.Unsafe.unsafePerformIO` if you won't get control back again (like, in a GHC plugin).
--
-- This works by wrapping the `Exception` in a newtype that has the correct behavior, which means
-- that if you want to try handling these exceptions outside a call to `displayExceptions`, you
-- should be using `handleIgnoringDisplay` (or `catchIgnoringDisplay`).
displayExceptions :: MonadUnliftIO m => m a -> m a
displayExceptions =
  -- Using `handleIgnoringDisplay` in order to avoid re-wrapping already-wrapped
  -- exceptions.
  handleIgnoringDisplay (\e -> Exception.throwIO (Display (e :: SomeException)))

-- | Handles an exception that /may/ be wrapped in the `Display` @newytpe@, unwrapping it if
--   necessary.
handleIgnoringDisplay :: (MonadUnliftIO m, Exception e) => (e -> m a) -> m a -> m a
handleIgnoringDisplay f = Exception.handle f . Exception.handle (\(Display e) -> f e)

-- | See `handleIgnoringDisplay`.
catchIgnoringDisplay :: (MonadUnliftIO m, Exception e) => m a -> (e -> m a) -> m a
catchIgnoringDisplay = flip handleIgnoringDisplay
