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
    $scope.term = Array([data]);
  });
  $scope.show_next_term = function(pickle) {
    console.log(pickle);
  }
});

txApp.directive('term', function ($compile) {
  return {
    restrict: "E",
    replace: true,
    scope: { term: '=' },
    template: '<div></div>',
    link: function (scope, element, attrs) {
      scope.$watch(attrs.term, function (x) {
        if (x) {
//          scope.$apply(function() {
//            var content = $compile(html_term(scope.term))(scope);
//            element.append(content);
//          });
          var compiled_templ = $compile(html_term(x));
          element.append(compiled_templ(scope));
        }
      });
    }
  }
});

function html_term(term) {
  var result = '';
  if (term.t == 't') {
    result += '<div class="tuplebox">{ ' + collapse_container();
    term.v.forEach(function (subterm) {
      result += '<div class="indent">' + html_term(subterm) + '</div>';
    });
    result += '}</div>';
  } else if (term.t == 'l') {
    result += '<div class="listbox">[ ' + collapse_container();
    term.v.forEach(function(subterm) {
      result += '<div class="indent">' + html_term(subterm) + '</div>';
    });
    result += ']</div>';
  } else if (term.t == 's') {
    if (term.v.length < 1024) {
      result += '&ldquo;<span class="value string">' + htmlq(term.v) +
          '</span>&rdquo;';
    } else {
      result += '<div class="value string">' + collapse_container()
          + htmlq(term.v) + '</div>';
    }
  } else if (term.t == 'a') {
    result += '&lsquo;<span class="value atom">' + term.v + '</span>&rsquo;';
  } else if (term.t == 'b') {
    if (term.v.length < 1024) {
      result += '<span class="binary"><span class="binaryblue value">' + htmlq(term.v) +
          '</span></span>';
    } else {
      result += '<div class="binary">' + collapse_container()
          + htmlq(term.v) + '</div>';
    }
  } else if (term.t == 'bs') {
    result += '<span class="binarystr"><span class="binaryblue value">' + htmlq(term.v) +
        '</span></span>';
  } else if (term.t == 'p') {
    result += '<span class="value pid">' +
        //'<button ng-click="show_next_term(\'' + term.pickle + '\')">' +
        term.v +
        //'</button>' +
        '</span>';
  } else if (term.t == 'r') {
    result += '<span class="value ref">' + term.v + '</span>';
  } else if (term.t == 'port') {
    result += '<span class="value port">' + term.v + '</span>';
  } else if (term.t == 'i') {
    result += '<span class="value integer">' + term.v + '</span>';
  } else if (term.t == 'f') {
    result += '<span class="value float">' + term.v + '</span>';
  } else if (term.t == 'fun') {
    result += '<span class="fun">' +
        '<span class="value">' + term.m + '</span>:' +
        '<span class="value">' + term.n + '</span>/' +
        '<span class="value">' + term.a + '</span></span>';
  } else {
    result += '<div class="value">' + term.v + '</div>';
  }
  return result;
}

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