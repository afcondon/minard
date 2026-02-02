-- | Minimal FFI bindings to Puppeteer using Control.Promise
-- | This module only wraps primitives - no business logic
module HalogenSpider.Spider.Puppeteer
  ( -- * Browser lifecycle (bracket pattern)
    withBrowser
  , withPage
  -- * Navigation
  , navigateTo
  , waitForRender
  -- * Link extraction
  , extractLinks
  -- * Configuration types
  , BrowserOptions
  , defaultBrowserOptions
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import HalogenSpider.Spider.Types (BrowserHandle, PageHandle, CssSelector(..))

-- | Browser launch options
type BrowserOptions =
  { headless :: Boolean
  , args :: Array String
  }

-- | Default browser options (headless, sandbox disabled for Docker compatibility)
defaultBrowserOptions :: BrowserOptions
defaultBrowserOptions =
  { headless: true
  , args: ["--no-sandbox", "--disable-setuid-sandbox"]
  }

-- Foreign imports using Promise
foreign import launchBrowserImpl :: Fn1 BrowserOptions (Effect (Promise BrowserHandle))
foreign import closeBrowserImpl :: Fn1 BrowserHandle (Effect (Promise Unit))
foreign import newPageImpl :: Fn1 BrowserHandle (Effect (Promise PageHandle))
foreign import closePageImpl :: Fn1 PageHandle (Effect (Promise Unit))
foreign import gotoImpl :: Fn3 PageHandle String Int (Effect (Promise Unit))
foreign import waitImpl :: Fn1 Int (Effect (Promise Unit))
foreign import extractLinksImpl :: Fn2 PageHandle String (Effect (Promise (Array String)))

-- | Launch a browser, run an action, then close the browser
-- | Uses bracket pattern for safe resource management
withBrowser :: forall a. BrowserOptions -> (BrowserHandle -> Aff a) -> Aff a
withBrowser opts action =
  bracket
    (toAffE $ runFn1 launchBrowserImpl opts)
    (\browser -> toAffE $ runFn1 closeBrowserImpl browser)
    action

-- | Create a page, run an action, then close the page
withPage :: forall a. BrowserHandle -> (PageHandle -> Aff a) -> Aff a
withPage browser action =
  bracket
    (toAffE $ runFn1 newPageImpl browser)
    (\page -> toAffE $ runFn1 closePageImpl page)
    action

-- | Navigate to a URL with timeout
navigateTo :: PageHandle -> String -> Int -> Aff Unit
navigateTo page url timeoutMs =
  toAffE $ runFn3 gotoImpl page url timeoutMs

-- | Wait for JavaScript rendering
waitForRender :: Int -> Aff Unit
waitForRender ms = toAffE $ runFn1 waitImpl ms

-- | Extract all links matching a CSS selector from the current page
-- | Returns raw href values (not normalized)
extractLinks :: PageHandle -> CssSelector -> Aff (Array String)
extractLinks page (CssSelector selector) =
  toAffE $ runFn2 extractLinksImpl page selector
