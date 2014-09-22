var txApp = angular.module('txApp', []);

txApp.controller('TxListCtrl', function ($scope, $http) {
  $http.get('/tx/tx_esi:list').success(function(data) {
    data.entries.sort(function(a,b) {return (a.created > b.created) ? -1 : ((b.created > a.created) ? 1 : 0);} );
    $scope.term_list = data.entries;
  });

  $scope.unix_ts_to_date = function(u) {
    var date = new Date(u*1000);
    var hours = date.getHours();
    var minutes = date.getMinutes();
    var seconds = date.getSeconds();

    var zerofill2 = function(x) { return ('00' + x).substr(-2) };

    return zerofill2(hours) + ':' + zerofill2(minutes) + ':' + zerofill2(seconds);
  };
});
