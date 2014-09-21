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
    var zerofill2 = function (x) {
      return ('00' + x).substr(-2)
    };
    return zerofill2(hours) + ':' + zerofill2(minutes) + ':' + zerofill2(seconds);
  };
});


txApp.controller('TxShowCtrl', function ($scope, $http) {
  $scope.show_id = window.location.hash.substring(1);
  $http.get('/tx/tx_esi:show?' + $scope.show_id).success(function(data) {
    $scope.term_stack = [data];
    $scope.get_term = function() {
      return $scope.term_stack[0];
    }
  });
  $scope.show_next_term = function(pickle) {
    console.log(pickle);
  }
});

function htmlq(q) {
  return q.replace(/</g, '&lt;').replace(/>/g, '&gt;')
      .replace(/\n/g, '<br/>\n').replace(/\t/g, '<span class="tab"></span>')
}

function collapse_container() {
  return '<button class="btn btn-xs btn-default minus" ' +
      'onclick="toggleCollapsed($(this), $(this).parent())"></button>';
}

function toggleCollapsed(button, collapsible) {
  collapsible.toggleClass('collapsed');
  if (collapsible.hasClass('collapsed')) {
    button.addClass('plus');
    button.removeClass('minus');
  } else {
    button.addClass('minus');
    button.removeClass('plus');
  }
}

function click_all_buttons(cls) {
  var root = $('div#terms');
  click_buttons_under(root, cls);
}

function click_buttons_under(root, cls) {
  $.each($(root).find('button.' + cls), function(index, value) {
    $(value).trigger('click');
  });
}

txApp.directive('termShow', function($compile){
  return {
    restrict: 'A',
    scope: { termData: '=' },
    link: function(scope, element, attrs){
      scope.$watch(function() { return scope.termData; }, function() {
        if (scope.termData) {
          element.append(term_template(scope.termData));
          $compile(element.contents())(scope);
        }
      });
    }
  }
});

function term_template(term) {
  if (term.t == 'l') {
    return '<div class="listbox">[ ' + collapse_container() +
        '<div term-collection nested-data="termData.v"></div>]</div>';
  } else if (term.t == 't') {
    return '<div class="tuplebox">{ ' + collapse_container() +
        '<div term-collection nested-data="termData.v"></div>}</div>';
  } else if (term.t == 's') {
    if (term.v.length < 1024) { // short strings
      return '&ldquo;<span class="value string">' + htmlq(term.v) +
          '</span>&rdquo;';
    } else { // long strings with collapse button
      return '<div class="value string">' + collapse_container() +
          htmlq(term.v) + '</div>';
    }
  } else if (term.t == 'a') {
    return '&lsquo;<span class="value atom">' + htmlq(term.v) +
        '</span>&rsquo;';
  } else if (term.t == 'b') {
    if (term.v.length < 1024) {
      return '<span class="binary"><span class="binaryblue value">' +
          htmlq(term.v) +
          '</span></span>';
    } else {
      return '<div class="binary">' + collapse_container() + htmlq(term.v) +
          '</div>';
    }
  } else if (term.t == 'bs') {
    return '<span class="binarystr"><span class="binaryblue value">' +
        htmlq(term.v) + '</span></span>';
  } else if (term.t == 'p') {
    return '<span class="value pid">' +
      //'<button ng-click="show_next_term(\'' + term.pickle + '\')">' +
      term.v +
      //'</button>' +
      '</span>';
  } else if (term.t == 'r') {
    return '<span class="value ref">' + term.v + '</span>';
  } else if (term.t == 'port') {
    return '<span class="value port">' + term.v + '</span>';
  } else if (term.t == 'i') {
    return '<span class="value integer">' + term.v + '</span>';
  } else if (term.t == 'f') {
    return '<span class="value float">' + term.v + '</span>';
  } else if (term.t == 'fun') {
    return '<span class="fun">' +
        '<span class="value">' + term.m + '</span>:' +
        '<span class="value">' + term.n + '</span>/' +
        '<span class="value">' + term.a + '</span></span>';
  }
  return '<div>{{termData}}</div>';
}

txApp.directive('termCollection', function(){
  return {
    restrict: 'A',
    replace: true,
    scope: {
      nestedData: '='
    },
    template: '<div class="indent" ng-repeat="subterm in nestedData">' +
        '<div term-show  term-data="subterm"></div></div>'
  };
});