var SoundApi = (function() {
  var context;
  var snds = [];

  function Sound(context, buffer)
  {
    var self = this;

    function play(volume, loop) {
      var source = self.source = context.createBufferSource();
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

    function stop() {
      if (self.source)
        self.source.stop();
    }

    return {
      play: play,
      stop: stop
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
    .then(function(snd) { snd.stop(); snd.play(volume, loop); })
});
app.ports.saveHighScore.subscribe(function(score) {
  saveScoreIfNewBest(score);
});
app.ports.getHighScore.subscribe(function() {
  score = getHighScore();
  app.ports.updateHighScore.send(score);
});
