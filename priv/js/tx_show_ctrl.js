var txApp = angular.module('txApp', []);

txApp.controller('TxShowCtrl', function ($scope, $http) {
  var stored_id = getQueryParam('stored', false);
  if (stored_id) {
    $scope.show_id = 'stored=' + stored_id;
    $scope.show_id_title = '(stored) ' + stored_id;
    $scope.inspect_mode = false;
  } else {
    var inspect_id = getQueryParam('inspect', undefined);
    $scope.show_id = 'inspect=' + inspect_id;
    $scope.show_id_title = '(inspecting live) ' +
        decodeURIComponent(getQueryParam('repr', inspect_id));
    $scope.inspect_mode = true;
  }
  // query server, it will give us parsed tree and raw string data
  $http.get('/tx/tx_esi:show?' + $scope.show_id).success(function (data) {
    $scope.term_to_show = data;
  });

  $scope.clickAllButtons = function(cls) {
    var root = $('div#terms');
    $scope.clickButtonsUnder(root, cls);
  };

  $scope.clickButtonsUnder = function(root, cls) {
    $.each($(root).find('button.' + cls), function(index, value) {
      $(value).trigger('click');
    });
  }
}).config(function($locationProvider) {
  $locationProvider.html5Mode(true).hashPrefix('!');
});

// called from DOM +/- buttons
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

function html_quote(text) {
  if (!text) return "";
  return text.replace(/</g, '&lt;').replace(/>/g, '&gt;')
      .replace(/\n/g, '<br/>\n').replace(/\t/g, '<span class="tab"></span>')
}

txApp.directive('termShow', function($compile){
  var collapse_container = function() {
    return '<button class="btn btn-xs btn-default minus" ' +
        'onclick="toggleCollapsed($(this), $(this).parent())"></button>';
  };

  var inspectTermBtn = function(pickle, value) {
    var qvalue = encodeURIComponent(value);
    var qpickle = encodeURIComponent(pickle);
    return '<a href="show.html?inspect=' + qpickle + '&repr=' +
        qvalue + '" target="_blank">' + html_quote(value) + '</a>';
  };

  var term_template = function (term) {
    if (term.t == 'l') {
      var result = '';
      if (term.tail_element) {
        result += '<div class="listbox improperlist">[ ';
      } else {
        result += '<div class="listbox">[ ';
      }
      result += collapse_container() +
          '<div term-collection nested-data="termData.v"></div>';
      if (term.tail_element) {
        result += '<div class="indent improperlisttail">' +
            '<div term-show term-data="termData.tail_element"></div>' +
            '</div>';
      }
      result += ']</div>';
      return result;
    } else if (term.t == 't') {
      return '<div class="tuplebox">{ ' + collapse_container() +
          '<div term-collection nested-data="termData.v"></div>}</div>';
    } else if (term.t == 'prop') {
      return '<div class="listbox">proplist [ ' + collapse_container() +
          '<div term-proplist nested-data="termData.v"></div>]</div>';
    } else if (term.t == 'm') {
      return '<div class="listbox">map #{ ' + collapse_container() +
          '<div term-proplist nested-data="termData.v"></div>}</div>';
    } else if (term.t == 's') {
      if (term.v.length < 1024) { // short strings
        return '&ldquo;<span class="value string">' + html_quote(term.v) +
            '</span>&rdquo;';
      } else { // long strings with collapse button
        return '<div class="value string">' + collapse_container() +
            html_quote(term.v) + '</div>';
      }
    } else if (term.t == 'a') {
      return '&lsquo;<span class="value atom">' + html_quote(term.v) +
          '</span>&rsquo;';
    } else if (term.t == 'b') {
      if (term.v.length < 1024) {
        return '<span class="binary"><span class="binaryblue value">' +
            html_quote(term.v) +
            '</span></span>';
      } else {
        return '<div class="binary">' + collapse_container() + html_quote(term.v) +
            '</div>';
      }
    } else if (term.t == 'bs') {
      return '<span class="binarystr"><span class="binaryblue value">' +
          html_quote(term.v) + '</span></span>';
    } else if (term.t == 'p') {
      return '<span class="value pid">' + inspectTermBtn(term.pickle, term.v) +
        '</span>';
    } else if (term.t == 'r') {
      return '<span class="value ref">' + term.v + '</span>';
    } else if (term.t == 'port') {
      return '<span class="value port">' + inspectTermBtn(term.pickle, term.v) +
        '</span>';
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
  };

  return {
    restrict: 'A',
    scope: {
      termData: '='
    },
    link: function(scope, element, attrs){
      scope.showNextTerm = scope.$parent.showNextTerm;
      scope.$watch(function() { return scope.termData; }, function() {
        if (scope.termData) {
          element.append(term_template(scope.termData));
          $compile(element.contents())(scope);
        }
      });
    }
  }
});

txApp.directive('termCollection', function(){
  return {
    restrict: 'A',
    replace: true,
    scope: {
      nestedData: '='
    },
    template: '<div class="indent" ng-repeat="subterm in nestedData">' +
        '<div term-show  term-data="subterm"></div></div>',
    link: function(scope, element, attrs) {
      scope.showNextTerm = scope.$parent.showNextTerm;
    }
  };
});

txApp.directive('termProplist', function(){
  return {
    restrict: 'A',
    replace: true,
    scope: {
      nestedData: '='
    },
    template: '<div class="indent" ng-repeat="proplistitem in nestedData">' +
        '<div class="row proplist">' +
        '<div class="col-lg-2 propkey"><div term-show term-data="proplistitem.k"></div></div>' +
        '<div class="col-lg-10"><div term-show term-data="proplistitem.v"></div></div>' +
        '</div></div>',
    link: function(scope, element, attrs) {
      scope.showNextTerm = scope.$parent.showNextTerm;
    }
  };
});

function getQueryParam(name, dflt) {
  var url = window.location.search.substring(1);
  var query = url.split('&');
  for(var i = 0; i < query.length; i++) {
    var key_val = query[i].split('=');
    if (key_val[0] == name) {
      return key_val[1];
    }
  }
  return dflt;
}