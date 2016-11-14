var breakin = breakin || {};

breakin.history = (function () {
  max_frames = 4000;
  current = 0;
  undoHistory = [];

  function rewind(frames) {
    if (current - frames < 0) {
      current = 0;
    } else {
      current -= frames;
    }
    return undoHistory[current];
  }

  function fastForward(frames) {
    if (current + frames >= undoHistory.length) {
      current = undoHistory.length - 1;
    } else {
      current = current + frames;
    }
    return undoHistory[current];
  }

  function saveState(state) {
    undoHistory[current++] = state;
    while (current > max_frames) {
      undoHistory = undoHistory.slice(1);
      current--;
    }
  }

  return {
    saveState: saveState,
    rewind: rewind,
    fastForward: fastForward
  }
})();

var SoundApi = (function() {
  var context;
  var snds = [];

  function Sound(context, buffer)
  {
    function play(volume, loop) {
      var source = context.createBufferSource();
      source.buffer = buffer;

      // looping
      source.loop = !!loop;

      // volume
      var gainNode = context.createGain();
      source.connect(gainNode);
      gainNode.gain.value = (volume == undefined) ? 1 : volume;
      gainNode.connect(context.destination);

      source.start(0);
    }

    return {
      play: play
    }
  }

  function init() {
    return new Promise(function(resolve, reject) {
      if (context) resolve(context);

      try {
        // needed for Safari
        window.AudioContext = window.AudioContext || window.webkitAudioContext;

        context = new AudioContext();

        resolve(context);
      } catch(err) {
        reject(err);
      }
    });
  }

  function loadSound(context, url) {
    return new Promise(function (resolve, reject) {
      if (snds[url]) resolve(snds[url]);

      var request = new XMLHttpRequest();
      request.open('GET', url, true);
      request.responseType = "arraybuffer";

      request.onload = function() {
        context.decodeAudioData(request.response, function(buffer) {
          snds[url] = new Sound(context, buffer);
          resolve(snds[url]);
        }, function(err) {
          reject(error);
        });
      }

      request.send();
    });
  }

  return {
    init: init,
    loadSound: loadSound
  };

})();

function getHighScore() {
  if (localStorage.highScore)
    return +localStorage.highScore;
  return 0;
}
function saveScoreIfNewBest(score) {
  best = getHighScore();
  if (score > best) {
    localStorage.highScore = score;
    app.ports.updateHighScore.send(score);
  }
}

var node = document.getElementById('main');
var app = Elm.Main.embed(node);
app.ports.playSound.subscribe(function (params) {
  var volume = params[0];
  var loop = params[1];
  var filename = params[2];
  SoundApi.init()
    .then(function(context) { return SoundApi.loadSound(context, filename); })
    .then(function(snd) { snd.play(volume, loop); })
});
app.ports.saveState.subscribe(function (state) {
  breakin.history.saveState(state);
});
app.ports.rewind.subscribe(function () {
  app.ports.updateModel.send(breakin.history.rewind(1));
});
app.ports.fastforward.subscribe(function() {
  app.ports.updateModel.send(breakin.history.fastForward(1));
});
app.ports.saveHighScore.subscribe(function(score) {
  saveScoreIfNewBest(score);
});
app.ports.getHighScore.subscribe(function() {
  score = getHighScore();
  app.ports.updateHighScore.send(score);
});
