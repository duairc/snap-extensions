# Snap Extentions

Snap Extensions is a library which makes it easy to extend your Snap
application with modular chunks of functionality such as session management,
user authentication, templating and database connection pooling.

## Included Extensions

<dl>
  <dt><code>Snap.Extension.DBPool</code></dt>
  <dd>Gives applications which need a HDBC `Connection` efficient pooling for
    free.
  </dd>
  <dt><code>Snap.Extension.Heist</code></dt>
  <dd>Integrate
    <a href="http://snapframework.com/docs/tutorials/heist">Heist</a>
    templating into your application.</dd>
  <dt><code>Snap.Extension.Less</code></dt>
  <dd>Integrate <a href="http://lesscss.org">Less</a> stylesheets into your
    application.
  </dd>
</dl>

## Using Snap Extensions

Every extension has an interface and at least one implementation of that
interface. For some extensions, like Heist, there is only ever going to be
one implementation of the interface. In these cases, the interface and the
implementation thereof are exported from the same module,
`Snap.Extension.Heist`. For something like session management though, there
could be multiple implementations, one using a HDBC backend, one using a
CouchDB backend and one just using a flat-file backend. In these cases, the
interface is exported from `Snap.Extension.Session`, and the implementations
live in `Snap.Extension.Session.HDBC`, `Snap.Extension.Session.CouchDB` and
`Snap.Extension.Session.FlatFile`.

Keeping this in mind, there are a number of things you need to do to use
Snap extensions in your application.

### Define application State and Monad

First, we define a record type `AppState` for holding our application's state,
including the state needed by the extensions we're using.

At the same time, we also define the monad for our application, `App`, as a
type alias to `SnapExtend AppState`. `SnapExtend` is a `MonadSnap` and a
`MonadReader`, whose environment is a user-supplied type; in our case,
`AppState`.

    module App where
    
    import Database.HDBC.Postgresql
    import Snap.Extension
    import Snap.Extension.DBPool
    import Snap.Extension.Heist
    import Snap.Types
    
    type App = SnapExtend AppState
    
    data AppState = AppState
        { dbPoolState :: DBPoolState Connection
        , heistState  :: HeistState App
        }

An important thing to note is that the `-State` types that we use in the 
fields of `AppState` are specific to each implementation of a extension's
interface. That is, `Snap.Extension.Session.HDBC` will export a different
`SessionState` to `Snap.Extension.Session.FlatFile`, whose internal
representation might be complete different.

### Provide instances for "`HasState`" classes

So, we have a datatype that contains all the internal state needed by our
application and the extensions it uses. Great! But when do we actually
get to use this interface that these extensions export? What is actually being
extended?

We use the interface provided by an extension inside our application's monad,
`App`. Snap extensions extend our `App` with new functionality. For example,
the Heist extension provides the function
`render :: MonadHeist m => ByteString -> m ()`. Is `App` a `MonadHeist`? Well,
not quite yet. Any `MonadReader` which is also a `MonadSnap` whose environment
contains a `HeistState` is a `MonadSnap`. That sounds a lot like `App`,
doesn't it? We just have to tell the Heist extension how to find the
`HeistState` in our `AppState`.

    instance HasHeistState AppState where
        getHeistState = heistState
        setHeistState hs as = as { heistState = hs }

And similarly for our `DBPoolState`:

    instance HasDBPoolState AppState where
        getDBPoolState = dbPoolState
        setDBPoolState dbps as = as { dbPoolState = dbps }

With these instances, our application's monad `App` is now a `MonadHeist` and
a `MonadDBPool`, giving it access to operations like
`render :: MonadHeist m => ByteString -> m ()` and
`withConnection :: MonadDBPool m => (Connection -> IO a) -> m a`.

### Define the Runner for `AppState`

So, our monad is now a `MonadHeist` and a `MonadDBPool`, but how do we
actually construct our `AppState` and turn an `App ()` into a `Snap ()`? Snap
extensions has a thing called a "Runner" that does these things. Each
implementation of a Snap extenion interface provides a Runner for its `-State`
type. We must construct a runner type for our `-State` type, `AppState`. A
`Runner` monad is provided to make it easy to do this. For your convenience,
`Runner` is an instance of `MonadIO`.

    import Text.Templating.Heist
    
    appRunner :: Runner AppState
    appRunner = do
        db <- dbPoolRunner $ connectPostgreSQL "user=dbuser pass=sekrit"
        hs <- heistRunner "resources/templates" emptyTemplateState
        return $ AppState db hs

In addition to constructing the `AppState`, the `Runner` monad also constructs
the init, destroy and reload functions for our application from the init,
reload and destroy functions for the extensions. Although it won't cause a
compile-time error, it is important to get the order of the runners correct as
much as possible, otherwise they may be reloaded and destroyed in the wrong
order. The "right" order is an order where every extension's dependencies are
initialised before that extension. For example,
`Snap.Extension.Session.HDBC` would depend on something which would extend the
monad with `MonadDBPool`, i.e., `Snap.Extension.DBPool`. If you had this
configuration it would be important that you put the `dbPoolRunner` before
the `sessionRunner` in your `appRunner`.

This `Runner AppState` can then be passed to `runRunner`, whose type signature
is
`Runner s -> SnapExtend s () -> IO (Snap (), IO (), IO [(ByteString, Maybe ByteString)])`.
The other arguments it takes is an `App ()`, and the tuple it returns is a
`Snap` action (which can be passed to `httpServe`), a cleanup action (which
you run after `httpServe`) and a "reload" action (which you may want to use
in your handler for the path "admin/reload"; the list it returns is for error
reporting - there is one tuple in the list for each Snap extension; the first
element of the tuple is the name of the Snap extension and the second is a
`Maybe` which contains `Nothing` if there was no error reloading that
extension and a `Just` with the `ByteString` containing the error message if
there was) and a cleanup action which you would run after `httpServe`. The
following is an example of how you might use this in `main`:

    main :: IO ()
    main = do
        (snap,cleanup,reload) <- runRunner appRunner site
        quickHttpServe $ snap
                      <|> path "admin/reload" $ defaultReloadHandler reload
        cleanup

You'll notice we're using `defaultReloadHandler`. This is a function exported
by `Snap.Extension` with the type signature
`MonadSnap m => IO [(ByteString, Maybe ByteString)] -> m ()`. It takes the
reload action returned by `runRunner` and returns a `Snap` action which
renders a simple page showing how the reload went.

### Hint

One of the big features of Snap 0.3 is the "development" mode which is
implemented using Hint. To use Hint in its current form, you need to call a
Template Haskell function `loadSnapTH` which resembles an imaginary function
`loadSnap :: IO AppState -> (AppState -> IO ()) -> (AppState -> Snap ()) -> IO (IO (), Snap ())`.
The tuple it returns contains a cleanup function and a `Snap` action which can
be served with `httpServe`.

To make it easier to use Snap extensions with the Hint project template in
0.3,
`runRunnerHint :: Runner s -> SnapExtend s () -> (IO [(ByteString, Maybe ByteString)] -> SnapExtend s ()) -> IO (IO s, s -> IO (), s -> Snap ())`
is provided. The first arguments are the same as in `runRunner`. The
additional third argument is to do with reload handling; if you would like the
same behaviour as the above example using `runRunner`, then you would pass
`path "admin/reload" . defaultReloadHandler` as that argument. If you don't
want to a web handler for reloading Snap Extensions at all, you could just
pass `nullReloadHandler` as the argument.

The tuple it returns contains all the arguments you need to pass to
`loadSnapTH`. An example of the use of `runRunnerHint` is given below:

    main :: IO ()
    main = do
        (state,mkCleanup,mkSnap) <-
            runRunnerHint appRunner site $ path "admin/reload" . defaultReloadHandler

        (cleanup,snap) <- $(loadSnapTH 'state 'mkCleanup 'mkSnap)
        quickHttpServe snap
        cleanup

## Developing Snap Extensions

See `src/Snap/Extensions/Heist.hs` for an example of a Snap extension.
