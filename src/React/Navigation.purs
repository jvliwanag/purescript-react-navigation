module React.Navigation ( Route(..)
                        , RouteConfig(..)
                        , Navigation
                        , NavProp
                        , ReactNavClass
                        , ReactNavClass'
                        , mkRoutes
                        , navigate
                        , getNavParams
                        , setNavParams
                        , createStackNavigator
                        , createSwitchNavigator
                        , class RoutesBuilder
                        , class RoutesBuilderRowList
                        , class Navigable
                        , class RouteMatcher
                        , class RouteMatcherRowList
                        ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Prim.Row (class Cons) as Row
import React (ReactClass, ReactThis, getProps)
import Type.Prelude (class RowToList)
import Type.Row (Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Navigation :: Type -> Type -> Type

data Route (name :: Symbol) param

type NavProp rts params = { navigation :: Navigation rts params }

type ReactNavClass rts params = ReactClass (NavProp rts params)
type ReactNavClass' = forall rts params. ReactNavClass rts params

newtype RouteConfig rts params = RouteConfig { screen :: ReactNavClass rts params }

foreign import getNavParamsD :: forall rts params. Navigation rts params -> Effect params
foreign import setNavParamsD :: forall rts params. Navigation rts params -> params -> Effect Unit
foreign import unsafeNavigate :: forall rts p params. Navigation rts p -> String -> params -> Effect Unit
foreign import unsafeCreateStackNavigator :: forall r configs. configs -> ReactClass r
foreign import unsafeCreateSwitchNavigator :: forall r configs. configs -> ReactClass r

getNavParams :: forall rts params state. ReactThis (NavProp rts params) state -> Effect params
getNavParams rt = withNavParam rt getNavParamsD

setNavParams :: forall rts params state. ReactThis (NavProp rts params) state -> params -> Effect Unit
setNavParams rt p = withNavParam rt (\nav -> setNavParamsD nav p)

withNavParam :: forall rts params state a. ReactThis (NavProp rts params) state -> (Navigation rts params -> Effect a) -> Effect a
withNavParam rt f = do
  props <- getProps rt
  f props.navigation

class RoutesBuilder a

instance routesBuilder :: (RowToList rows list, RoutesBuilderRowList list) => RoutesBuilder (Record rows)

class RoutesBuilderRowList (list :: RowList)
instance routesBuilderNil :: RoutesBuilderRowList Nil
instance routesBuilderCons :: (RoutesBuilderRowList t) => RoutesBuilderRowList (Cons n (Route name param) t)

mkRoutes :: forall rts. RoutesBuilder rts => rts
mkRoutes = unsafeCoerce {}

class Navigable (name :: Symbol) param rs | name param -> rs
instance navigable :: (IsSymbol name, Row.Cons name (Route name param) trash r) => Navigable name param {|r}

navigate :: forall rts p name params. IsSymbol name => Navigable name params rts => Navigation rts p -> Route name params -> params -> Effect Unit
navigate nav r params = unsafeNavigate nav (routeName r) params

routeName :: forall name params. (IsSymbol name) => Route name params -> String
routeName _ = reflectSymbol (SProxy :: SProxy name)

class RouteMatcher rts cfgs | rts -> cfgs
instance routeMatcher :: (RowToList rts rtRs, RowToList cfgs cfgRs, RouteMatcherRowList rts rtRs cfgRs) => RouteMatcher (Record rts) (Record cfgs)

class RouteMatcherRowList (rts :: # Type) (rtRs :: RowList) (cfgRs :: RowList) | rts rtRs -> cfgRs
instance routeMatcherNil :: RouteMatcherRowList rts Nil Nil
instance routeMatcherCons :: (RouteMatcherRowList rts rtT cfgT) => RouteMatcherRowList rts (Cons n (Route name param) rtT) (Cons name (RouteConfig (Record rts) param) cfgT)

createStackNavigator :: forall r rts cfgs
                        . (RouteMatcher rts cfgs)
                        => rts
                        -> cfgs
                        -> ReactClass r
createStackNavigator _ cfgs = unsafeCreateStackNavigator cfgs

createSwitchNavigator :: forall r rts cfgs
                         . (RouteMatcher rts cfgs)
                         => rts
                         -> cfgs
                         -> ReactClass r
createSwitchNavigator _ cfgs = unsafeCreateSwitchNavigator cfgs
