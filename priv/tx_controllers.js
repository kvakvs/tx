var txApp = angular.module('txApp', []);

txApp.controller('TxListCtrl', function ($scope, $http) {
  $http.get('/tx/tx_esi:list').success(function(data) {
    $scope.term_list = data.entries;
  });

  $scope.unix_ts_to_date = function(u) {
    var date = new Date(u*1000);
    var hours = date.getHours();
    var minutes = date.getMinutes();
    var seconds = date.getSeconds();
    return hours + ':' + minutes + ':' + seconds;
  };
});

txApp.controller('TxShowCtrl', function ($scope, $http) {
  $scope.show_id = window.location.hash.substring(1);

  $http.get('/tx/tx_esi:show?' + $scope.show_id).success(function(data) {
    $scope.term = data;
  });
});

txApp.directive('collection', function () {
  return {
    restrict: "E",
    replace: true,
    scope: { collection: '=' },
    template: "<ul><member ng-repeat='member in collection' member='member'></member></ul>"
  }
});

txApp.directive('member', function ($compile) {
  return {
    restrict: "E",
    replace: true,
    scope: { member: '=' },
    template: "<li>t={{member.t}} v={{member.v}}</li>",
    link: function (scope, element, attrs) {
      if (scope.member && angular.isArray(scope.member.v)) {
        element.append("<collection collection='member.v'></collection>");
        $compile(element.contents())(scope)
      }
    }
  }
});