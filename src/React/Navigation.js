var rnNavigation = require('react-navigation');

exports.getNavParamsD = function(navigation) {
  return function() {
    return navigation.state.params;
  }
};

exports.setNavParamsD = function(navigation) {
  return function(params) {
    return function () {
      navigation.setParams(params);
    };
  };
};

exports.unsafeNavigate = function(navigation) {
  return function(routeName) {
    return function(params) {
      return function() {
        navigation.navigate(routeName, params);
      };
    };
  };
};

exports.unsafeCreateStackNavigator = function (configs) {
  return rnNavigation.StackNavigator(configs);
}

exports.unsafeCreateSwitchNavigator = function (configs) {
  return rnNavigation.SwitchNavigator(configs);
}
