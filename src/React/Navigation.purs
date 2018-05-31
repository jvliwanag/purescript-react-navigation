module React.Navigation ( Route(..)
                        , RouteConfig(..)
                        , Navigation
                        , NavProp
                        , NavRender
                        , NavRender'
                        , ReactNavClass
                        , ReactNavClass'
                        , ReactNavSpec
                        , ReactNavSpec'
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

import Control.Monad.Eff (Eff)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import React (ReactClass, ReactProps, ReactSpec, ReactThis, Render, getProps)
import Type.Prelude (class RowToList)
import Type.Row (Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Navigation :: Type -> Type -> Type

data Route (name :: Symbol) param

type NavProp rts params = { navigation :: Navigation rts params }

type ReactNavClass rts params = ReactClass (NavProp rts params)
type ReactNavClass' = forall rts params. ReactNavClass rts params

newtype RouteConfig rts params = RouteConfig { screen :: ReactNavClass rts params }

type ReactNavSpec rts params state render eff =
  ReactSpec (NavProp rts params) state render eff

type ReactNavSpec' rts state render eff =
  ReactNavSpec rts {} state render eff

type NavRender rts params state render eff = Render (NavProp rts params) state render eff
type NavRender' rts state render eff = NavRender rts {} state render eff

foreign import getNavParamsD :: forall rts params eff. Navigation rts params -> Eff eff params
foreign import setNavParamsD :: forall rts params eff. Navigation rts params -> params -> Eff eff Unit
foreign import unsafeNavigate :: forall rts p params eff. Navigation rts p -> String -> params -> Eff eff Unit
foreign import unsafeCreateStackNavigator :: forall r configs. configs -> ReactClass r
foreign import unsafeCreateSwitchNavigator :: forall r configs. configs -> ReactClass r

getNavParams :: forall rts params state eff. ReactThis (NavProp rts params) state -> Eff (props :: ReactProps | eff) params
getNavParams rt = withNavParam rt getNavParamsD

setNavParams :: forall rts params state eff. ReactThis (NavProp rts params) state -> params -> Eff (props :: ReactProps | eff) Unit
setNavParams rt p = withNavParam rt (\nav -> setNavParamsD nav p)

withNavParam :: forall rts params state a eff. ReactThis (NavProp rts params) state -> (Navigation rts params -> Eff (props :: ReactProps | eff) a) -> Eff (props :: ReactProps | eff) a
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
instance navigable :: (IsSymbol name, RowCons name (Route name param) trash r) => Navigable name param (Record r)

navigate :: forall rts p name params eff. IsSymbol name => Navigable name params rts => Navigation rts p -> Route name params -> params -> Eff eff Unit
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
