// http://www.infiniteloop.co.jp/blog/2013/08/osx_slate/
var util = {
  // Alt + hoge
  key: function(k, mod) {
    return k + ':alt' + (mod ? ',' + mod : '');
  },
  focusWindow: function(f) {
    var hit = false;
    slate.eachApp(function(app) {
      if (hit) return;
      app.eachWindow(function(win) {
        if (hit) return;
        if (f(win)) {
          win.focus();
          hit = true;
        }
      });
    });
  },
  nextScreen: function(screen) {
    return slate.screenForRef(String( (screen.id()+1)%slate.screenCount() ));
  }
};


// Alt + U 左右に飛ばす
slate.bind(util.key('u'), slate.operation('chain', {
  operations: _.map(['left', 'right'], function(d) {
    return slate.operation('push', {
      direction: d,
      style: 'bar-resize:screenSizeX/2'
    });
  })
}));
 

// Alt + I 上下に飛ばす
slate.bind(util.key('i'), slate.operation('chain', {
  operations: _.map(['top', 'bottom'], function(d) {
    return slate.operation('push', {
      direction: d,
      style: 'bar-resize:screenSizeY/2'
    });
  })
}));


// Alt + O 4隅に飛ばす
var corners = slate.bind(util.key('o'), slate.operation('chain', {
  operations: _.map(['top-right', 'bottom-right', 'bottom-left', 'top-left'], function(d) {
    return slate.operation('corner', {
      direction: d,
      width: 'screenSizeX/2',
      height: 'screenSizeY/2'
    });
  })
}));
  

// Alt + P 次のスクリーンへ飛ばす
slate.bind(util.key('p'), function(win) {
  if (!win) return;
  var next = util.nextScreen(win.screen());
  win.move(next.visibleRect());
});


// Alt + M 最大化
slate.bind(util.key('m'), function(win) {
  if (!win) return;
  var bounds = win.screen().visibleRect();
  win.doOperation('move', bounds);
});


// http://mint.hateblo.jp/entry/2013/01/29/003541
// http://yohasebe.com/wp/archives/3513

var topLeft = slate.operation("corner", {
  "direction" : "top-left",
  "width"  : "screenSizeX/2",
  "height" : "screenSizeY/2"
});
  
var topRight = slate.operation("corner", {
  "direction" : "top-right",
  "width"  : "screenSizeX/2",
  "height" : "screenSizeY/2"
});
  
var bottomRight = slate.operation("corner", {
  "direction" : "bottom-right",
  "width"  : "screenSizeX/2",
  "height" : "screenSizeY/2"
});
  
var bottomLeft = slate.operation("corner", {
  "direction" : "bottom-left",
  "width"  : "screenSizeX/2",
  "height" : "screenSizeY/2"
});


// [tab]+alt+shiftでアプリのウィンドウをタイル状に並べる
var tileKey = "tab:alt;shift";
  
slate.bind(tileKey, function(win){
  var appName = win.app().name();    
  var tiled = {};
  tiled[appName] = {
    "operations" : [topLeft, topRight, bottomRight, bottomLeft],
    "main-first" : true,
    "repeat"     : true
  };      
  var tiledLayout = slate.layout("tiledLayout", tiled);
  slate.operation("layout", {"name" : tiledLayout }).run();
  slate.operation("show", {"app" : appName}).run();
});

// http://d.hatena.ne.jp/sugyan/20130301/1362129310
// アプリ立ち上げる関数
var launch_and_focus = function (target) {
    return function (win) {
        var apps = [];
        S.eachApp(function (app) { apps.push(app.name()); });
        if (! _.find(apps, function (name) { return name === target; })) {
            win.doOperation(
                S.operation('shell', {
                    command: "/usr/bin/open -a " + target,
                    waithFoeExit: true
                })
            );
        }
        win.doOperation(S.operation('focus', { app: target }));
    };
};

S.bind('t:alt', launch_and_focus('iTerm'));
S.bind('c:alt', launch_and_focus('Google Chrome'));
S.bind('f:alt', launch_and_focus('Finder'));
S.bind('d:alt', launch_and_focus('Dictionary'));

