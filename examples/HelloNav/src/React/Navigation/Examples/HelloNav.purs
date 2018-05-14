module React.Navigation.Examples.HelloNav where

import Prelude

import Control.Monad.Eff.Uncurried (mkEffFn1)
import React (ReactClass, ReactElement, createClass, createClassStateless, spec)
import React.Navigation (class Navigable, NavProp, Navigation, ReactNavClass, Route, RouteConfig(..), createStackNavigator, getNavParams, mkRoutes, navigate)
import ReactNative.Components.Button (button)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view)
import ReactNative.PropTypes (center)
import ReactNative.Styles (Styles, flex, staticStyles)
import ReactNative.Styles.Flex (alignItems, justifyContent)

-- Step 1: Declare a record type for the routes
-- Each route takes in a symbol and a type for
-- the name and paramater respectively.

type HelloRoutes =
  { home :: Route "home" Unit
  , greetPerson :: Route "greetPerson" Unit
  , greetName :: Route "greetName" String
  }

-- Step 2: Create an instance of the route
routes :: HelloRoutes
routes = mkRoutes

-- Step 3: Create the navigator, passing the config
-- for each route. This creates a react class.
app :: ReactClass Unit
app = createStackNavigator routes $
  -- The keys of the record should follow the symbols
  -- defined in step 1. Not doing so will produce a compile error.
  -- The screen for each config should use the same route config
  -- or be compatible. In this example, they should should be
  -- ReactNavClass HelloRoutes Unit.
  { home: RouteConfig { screen: home
                      }
  , greetPerson: RouteConfig { screen: greetPerson
                             }
  , greetName: RouteConfig { screen: greetName
                           }
  }

-- Step 4: Define the route classes.
-- ReactNavClass rts param = ReactClass (NavProp rts params)
-- It is important to define the routes this route will belong to
-- since the type of the routes determine which routes can be navigated to.
home :: ReactNavClass HelloRoutes Unit
home = createClassStateless $ \p ->
  view sheet.container $
  [ button "Greet Someone" $ mkEffFn1 $ const $
    -- In order to navigate, get the navigation from props, and use the
    -- route defined in step 1. Take note that the routes navigable from
    -- the current route is limited to HelloRoutes.
    navigate p.navigation routes.greetPerson unit
  , button "Greet World" $ mkEffFn1 $ const $
    navigate p.navigation routes.greetName "World"
  ]

greetPerson :: ReactNavClass HelloRoutes Unit
greetPerson = createClassStateless $ \p ->
  view sheet.container $
  [ mkGreetBtn p.navigation "Alice"
  , mkGreetBtn p.navigation "Bob"
  ] where

    -- A simple declaration could be:
    -- mkGreetBtn :: forall p. Navigation HelloRoutes p -> String -> ReactElement
    -- But the more general declaration is as follows:
    mkGreetBtn :: forall rts p
                  . (Navigable "greetName" String rts)
                  => Navigation rts p
                  -> String
                  -> ReactElement
    mkGreetBtn nav p =
      button ("Greet" <> p) $ mkEffFn1 $ const $
      navigate nav routes.greetName p

greetName :: ReactNavClass HelloRoutes String
greetName = createClass $ spec unit render
  where
    render rt = do
      param <- getNavParams rt
      pure $ view sheet.container $ [ text_ $ "Hello " <> param
                                    ]

sheet ::
  { container :: Styles
  }
sheet =
  { container: staticStyles [ flex 1
                            , alignItems center
                            , justifyContent center
                            ]
  }
