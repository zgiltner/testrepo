const getCurrentStateKey = () =>
  parseInt(
    document.getElementById("gameState")?.getAttribute("data-state-key")
  );

htmx.defineExtension("game-state-ws", {
  transformResponse: function (resp, xhr, elt) {
    try {
      const { events, html, stateKey, chanMsg } = JSON.parse(resp);
      switch (chanMsg) {
        case "NonStateChangeMsg": {
          if (getCurrentStateKey() !== stateKey) {
            return null;
          }
          break;
        }
        case "AppGameStateChanged": {
          if (stateKey <= getCurrentStateKey()) {
            return null;
          }
          break;
        }
        default:
          throw new Error(`Unrecognized chanMsg: ${chanMsg}`);
      }
      if (events) {
        console.log(events);
        for (const event of events) {
          if (event) htmx.trigger("body", event);
        }
      }
      return html;
    } catch {
      return resp;
    }
  },
  onEvent: function (name, evt) {
    if (name === "htmx:wsConfigSend") {
      evt.detail.parameters.stateKey = getCurrentStateKey();
    }
  },
});

const tick = new Audio("/static/sounds/Buttons and Navigation/Button 5.m4a");

const eventSoundDict = {
  WrongGuess: new Audio("/static/sounds/Errors and Cancel/Cancel 1.m4a"),
  CorrectGuess: new Audio("/static/sounds/Complete and Success/Success 2.m4a"),
  MyTurn: new Audio("/static/sounds/Notifications and Alerts/Alert 3.m4a"),
  TimeUp: new Audio("/static/sounds/Errors and Cancel/Error 5.m4a"),
  IWin: new Audio("/static/sounds/Notifications and Alerts/Notification 9.m4a"),
  ILose: new Audio("/static/sounds/Errors and Cancel/Error 4.m4a"),
};
for (const event in eventSoundDict) {
  htmx.on(event, () => {
    eventSoundDict[event].play();
  });
}

let turnTickingIntervalID;
htmx.on("MyTurn", () => {
  clearInterval(turnTickingIntervalID);
  turnTickingIntervalID = setInterval(() => {
    tick.play();
  }, 1200);
});
htmx.on("TimeUp", () => {
  clearInterval(turnTickingIntervalID);
});
htmx.on("CorrectGuess", () => {
  clearInterval(turnTickingIntervalID);
});
htmx.on("GameOver", () => {
  clearInterval(turnTickingIntervalID);
});
htmx.on("SettingsUpdate", () => {
  clearInterval(turnTickingIntervalID);
});
