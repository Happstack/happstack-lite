Happstack is a powerful web framework with a rich API that has evolved
over the last 7 years to meet the needs of real world web
development. Unfortunately a rich and flexible API can also be
daunting when all you need is something simple. What many people don't
realize is that inside Happstack lives a very simple and easy to use
web framework.

happstack-lite brings that simple, easy to use version of Happstack to
light, without forcing you to give up any of the power and flexible.

To create happstack-lite, we have 

 1. Gathered all the essential types and functions you need to develop
a web application into a single module `Happstack.Lite` so you don't
have to hunt around for what you need.

 2. Given the functions much simpler type signatures by eliminating
monad transformers, getting rid of most of the type classes, etc.

 3. Created this tutorial which demonstrates, in less than 2000 words,
all the essential things you need to know to get started writing a web
application.

But, here is the best part. happstack-lite is 100% compatible with
Happstack. If you are developing an application using happstack-lite,
and you need an advanced feature from Happstack, you can simple import
the corresponding module and use it! To switch from happstack-lite to
regular happstack simply change your import from `Happstack.Lite` to
`Happstack.Server` and change `serve` to `simpleHTTP`. That's it!

While happstack-lite is 'lite' compared to regular Happstack, it is
still a full featured framework on par with other Haskell web
frameworks.

This document is a literate Haskell file. That means save this file
and run it with runhaskell!

The latest copy of this tutorial is located at:

http://patch-tag.com/r/mae/happstack/snapshot/current/content/pretty/happstack-lite/Examples.lhs

The latest HTML-ized version of this document is located at:

<a href="http://happstack.com/docs/happstack-lite/happstack-lite.html">http://happstack.com/docs/happstack-lite/happstack-lite.html</a>

The Haddock documentation for happstack-lite is located at:

<a href="http://happstack.com/docs/6.0.0/happstack-lite/index.html">http://happstack.com/docs/6.0.0/happstack-lite/index.html</a>

For more in-depth information, you can also refer to the Happstack Crash Course at:

<a href="http://www.happstack.com/docs/crashcourse/index.html">http://www.happstack.com/docs/crashcourse/index.html</a>

Now onto the tutorial. 

<ul>
 <li><a href="#starting">Starting the Server</a></li>
 <li><a href="#static">Static Routing</a></li>
 <li><a href="#templates">Templates</a></li>
 <li><a href="#path">Dynamic Path Segments</a></li>
 <li><a href="#query">Query String Parameters</a></li>
 <li><a href="#forms">Form Data</a></li>
 <li><a href="#cookies">Cookies</a></li>
 <li><a href="#fileserving">File Serving</a></li>
 <li><a href="#uploads">File Uploads</a></li>
</ul>

First we need some LANGUAGE pragmas:

> {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
> module Main where

And then we have some imports:

> import Control.Applicative ((<$>), optional)
> import Data.Maybe (fromMaybe)
> import Data.Text (Text)
> import Data.Text.Lazy (unpack)
> import Happstack.Lite
> import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
> import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A

<h3><a name="starting">Starting the Server</a></h3>

To start the app we call the `serve` function. The first argument
is an optional configuration parameter. The second argument is our web
application:

> main :: IO ()
> main = serve Nothing myApp

A web application has the type `ServerPart Response`. You can think of
`ServerPart` as the web equivalent of the `IO` monad. 

<h3><a name="static">Static Routing</a></h3>

Here is our web application:

> myApp :: ServerPart Response
> myApp = msum
>   [ dir "echo"    $ echo
>   , dir "query"   $ queryParams
>   , dir "form"    $ formPage
>   , dir "fortune" $ fortune
>   , dir "files"   $ fileServing
>   , dir "upload"  $ upload
>   , homePage
>   ]

The top-level of our application is just a bunch of routes mappings to handlers.

`dir` is guard used to match on static path components. For example, `dir "echo"`, will match on http://localhost:8000/echo. To match on `"/foo/bar"` you would write, `dir "foo" $ dir "bar" $ handler`.

Each of the routes is tried until one successfully returns a value. In this case, a `Response`.

We convert the list of handlers in a single handler using `msum`. 

The last handler, `homePage` is not guarded by anything, so it will always be called if none of the other handlers were successful.

<h3><a name="templates">HTML Templates</a></h3>

Since this is a web application, we are going to want to create some HTML pages. We will do that using blaze-html. A blaze tutorial can be found here:

<a href="http://jaspervdj.be/blaze/tutorial.html">http://jaspervdj.be/blaze/tutorial.html</a>

I like to make a template function which captures common elements of pages in my web app, such as importing style sheets, external javascript files, menus, etc. For this tutorial we have a very simple template:

> template :: Text -> Html -> Response
> template title body = toResponse $
>   H.html $ do
>     H.head $ do
>       H.title (toHtml title)
>     H.body $ do
>       body
>       p $ a ! href "/" $ "back home"
>

We can then use that template like this:

> homePage :: ServerPart Response
> homePage =
>     ok $ template "home page" $ do
>            H.h1 "Hello!"
>            H.p "Writing applications with happstack-lite is fast and simple!"
>            H.p "Check out these killer apps."
>            H.p $ a ! href "/echo/secret%20message"  $ "echo"
>            H.p $ a ! href "/query?foo=bar" $ "query parameters"
>            H.p $ a ! href "/form"          $ "form processing"
>            H.p $ a ! href "/fortune"       $ "(fortune) cookies"
>            H.p $ a ! href "/files"         $ "file serving"
>            H.p $ a ! href "/upload"        $ "file uploads"

`ok` tells the server to return the page with the HTTP response code '200 OK'. There are other helper functions like `notFound` and `seeOther` for other response codes. Or use `setResponseCode` to specify a response code by number.

<h3><a name="path">Dynamic Path Segments</a></h3>

The `dir` function only matches on static path segments. If we have a dynamic path segment, we can use the `path` function to capture the value and optionally convert it to another type such as Integer. In this example we just echo the captured path segment in the html. For example, trying visiting:

<a href="http://localhost:8000/echo/fantastic">http://localhost:8000/echo/fantastic</a>

> echo :: ServerPart Response
> echo =
>     path $ \(msg :: String) ->
>         ok $ template "echo" $ do 
>           p $ "echo says: " >> toHtml msg
>           p "Change the url to echo something else."

<h3><a name="query">Query String Parameters</a></h3>

We can also extract values from the query string part of the URL. The
query string is the part that looks like "?foo=bar". Trying visiting:

<a href="http://localhost:8000/query?foo=bar">http://localhost:8000/query?foo=bar</a>

> queryParams :: ServerPart Response
> queryParams =
>     do mFoo <- optional $ lookText "foo"
>        ok $ template "query params" $ do
>          p $ "foo is set to: " >> toHtml (show mFoo)
>          p $ "change the url to set it to something else."

`lookText` will normally fail (by calling `mzero`) if the parameter is not found. In this example we used `optional` from `Control.Applicative` so that it will return a `Maybe` value instead.

<h3><a name="forms">Form Data</a></h3>

We can use `lookText` (and friends) to extract values from forms as well.

> formPage :: ServerPart Response
> formPage = msum [ viewForm, processForm ]
>   where
>     viewForm :: ServerPart Response
>     viewForm =
>         do method GET
>            ok $ template "form" $
>               form ! action "/form" ! enctype "multipart/form-data" ! A.method "POST" $ do
>                 label ! A.for "msg" $ "Say something clever"
>                 input ! type_ "text" ! A.id "msg" ! name "msg"
>                 input ! type_ "submit" ! value "Say it!"
>
>     processForm :: ServerPart Response
>     processForm =
>         do method POST
>            msg <- lookText "msg"
>            ok $ template "form" $ do
>              H.p "You said:"
>              H.p (toHtml msg)
>               

We use the same `lookText` function from the previous section to look up values in in form data.

You will also note that we use the `method` function to select between a `GET` request and a `POST` request. 

When the user first views the form, the browser will request "/form"
using the `GET` method. In the form tag, we see that the form will
also be submitted to "/form" when the user presses the submit
button. But in the form tag we have set the method attribute to
`POST`.

<h3><a name="cookies">Cookies</a></h3>

This example extends the form example to save the message in a cookie. That means you can navigate away from the page and when you come back later it will remember the message you saved.

> fortune :: ServerPart Response
> fortune = msum [ viewFortune, updateFortune ]
>     where
>       viewFortune :: ServerPart Response
>       viewFortune =
>           do method GET
>              mMemory <- optional $ lookCookieValue "fortune"
>              let memory = fromMaybe "Your future will be filled with web programming." mMemory
>              ok $ template "fortune" $ do 
>                     H.p "The message in your (fortune) cookie says:"
>                     H.p (toHtml memory)
>                     form ! action "/fortune" ! enctype "multipart/form-data" ! A.method "POST" $ do
>                     label ! A.for "fortune" $ "Change your fortune: "
>                     input ! type_ "text" ! A.id "fortune" ! name "new_fortune"
>                     input ! type_ "submit" ! value "Say it!"
>
>       updateFortune :: ServerPart Response
>       updateFortune =
>           do method POST
>              fortune <- lookText "new_fortune"
>              addCookies [(Session, mkCookie "fortune" (unpack fortune))]
>              seeOther ("/fortune" :: String) (toResponse ())


There are only a few new things in this example compared to the form example.

 1. `lookCookieValue` works just like `lookText`, except it looks for the value in the cookies instead of request paramaters or form data.

 2. `addCookies` sends cookies to the browser. addCookies has the type:

     `addCookies :: [(CookieLife, Cookie)] -> ServerPart ()`

 3. `CookieLife` specifies how long the cookie is valid. `Session` means it is only valid until the browser window is closed.

 4. `mkCookie` takes the cookie name and the cookie value and makes a `Cookie`.

 5. `seeOther` (aka, 303 redirect) tells the browser to do a new GET request on "/fortune".

<h3><a name="fileserving">File Serving</a></h3>

In most web applications, we will want to serve static files from the
disk such as images, stylesheets, javascript, etc. We can do
that using the `serveDirectory` function.

> fileServing :: ServerPart Response
> fileServing =
>     serveDirectory EnableBrowsing ["index.html"] "."

The first argument specifies whether `serveDirectory` should create directory listings or not.

The second argument is a list of index files. If the user requests a directory and the directory contains a index file (in this example "index.html"), then the server will display that index file instead of a directory listing.

The third argument is the path to the directory we want to serve files from. Here we serve files from the current directory.

On support platforms (Linux, OS X, Windows), the `serveDirectory` function will automatically use sendfile() to serve the files. sendfile() uses low-level kernel operations to transfer files directly from the disk to the network with minimal CPU usage and maximal bandwidth usage.

<h3><a name="uploads">File Uploads</a></h3>

Handling file uploads is very straight forward. We create a form, just as before. Except instead of `lookText` we use `lookFile`.

> upload :: ServerPart Response
> upload = 
>        msum [ uploadForm 
>             , handleUpload
>             ]
>     where
>     uploadForm :: ServerPart Response
>     uploadForm = 
>         do method GET
>            ok $ template "upload form" $ do
>              form ! enctype "multipart/form-data" ! A.method "POST" ! action "/upload" $ do
>                input ! type_ "file" ! name "file_upload" ! size "40"
>                input ! type_ "submit" ! value "upload"
>
>     handleUpload :: ServerPart Response
>     handleUpload = 
>         do (tmpFile, uploadName, contentType) <- lookFile "file_upload"
>            ok $ template "file uploaded" $ do
>                 p (toHtml $ "temporary file: " ++ tmpFile)
>                 p (toHtml $ "uploaded name:  " ++ uploadName)
>                 p (toHtml $ "content-type:   " ++ show contentType)

When a file is uploaded, we store it in a temporary location. The
temporary file will automatically be deleted after the server has sent
the response. That ensures that unused files don't clutter up the
disk.

In most cases, you don't want a user to upload a file just to have it
deleted. Normally the upload handler would use `moveFile` or
`copyFile` to move (or copy) the temporary file to a permanent location.
