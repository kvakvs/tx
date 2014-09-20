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
    return hours + ':' + minutes + ':' + seconds;
  };
});

txApp.controller('TxShowCtrl', function ($scope, $http) {
  $scope.show_id = window.location.hash.substring(1);

  $http.get('/tx/tx_esi:show?' + $scope.show_id).success(function(data) {
    $scope.term = data;
  });
});

txApp.directive('term', function ($compile) {
//  var loop = '<listof collection="term.v"></listof>';
  return {
    restrict: "E",
    replace: true,
    scope: { term: '=' },
    template: '<div></div>',
    link: function (scope, element, attrs) {
      scope.$watch(attrs.term, function(x) {
        if (x) {
          element.append(html_term(x));
          $compile(element.contents())(scope);
        }
      });
    }
  }
});

function html_term(term) {
  var result = '';
  if (term.t == 't') {
    result += '<div class="tuplebox">{';
    term.v.forEach(function (subterm) {
      result += '<div class="indent">' + html_term(subterm) + '</div>';
    });
    result += '}</div>';
  } else if (term.t == 'l') {
    result += '<div class="listbox">[';
    term.v.forEach(function(subterm) {
      result += '<div class="indent">' + html_term(subterm) + '</div>';
    });
    result += ']</div>';
  } else if (term.t == 's') {
    result += '&ldquo;<span class="value string">' + term.v + '</span>&rdquo;';
  } else if (term.t == 'a') {
    result += '&lsquo;<span class="value atom">' + term.v + '</span>&rsquo;';
  } else if (term.t == 'b') {
    result += '<span class="value binary">' + term.v + '</span>';
  } else if (term.t == 'bs') {
    result += 'bits <span class="value binary">' + term.v + '</span>';
  } else if (term.t == 'p') {
    result += '<span class="value pid">' + term.v + '</span>';
  } else if (term.t == 'r') {
    result += 'ref <span class="value ref">' + term.v + '</span>';
  } else if (term.t == 'port') {
    result += 'port <span class="value port">' + term.v + '</span>';
  } else if (term.t == 'i') {
    result += '<span class="value integer">' + term.v + '</span>';
  } else if (term.t == 'f') {
    result += '<span class="value float">' + term.v + '</span>';
  } else {
    result += '<div class="value">' + term.v + '</div>';
  }
  return result;
}