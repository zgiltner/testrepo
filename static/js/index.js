htmx.defineExtension("transform-ws-response", {
  transformResponse: function (resp, xhr, elt) {
    try {
      const { events, html } = JSON.parse(resp);
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
});

const tick = new Audio("/static/sounds/Buttons and Navigation/Button 5.m4a");

const eventSoundDict = {
  wrongGuess: new Audio("/static/sounds/Errors and Cancel/Cancel 1.m4a"),
  correctGuess: new Audio("/static/sounds/Complete and Success/Success 2.m4a"),
  myTurn: new Audio("/static/sounds/Notifications and Alerts/Alert 3.m4a"),
  timeUp: new Audio("/static/sounds/Errors and Cancel/Error 5.m4a"),
  iWin: new Audio("/static/sounds/Notifications and Alerts/Notification 9.m4a"),
};
for (const event in eventSoundDict) {
  htmx.on(event, () => {
    eventSoundDict[event].play();
  });
}

let turnTickingIntervalID;
htmx.on("myTurn", () => {
  turnTickingIntervalID = setInterval(() => {
    tick.play();
  }, 1200);
});
htmx.on("timeUp", () => {
  clearInterval(turnTickingIntervalID);
});
htmx.on("correctGuess", () => {
  clearInterval(turnTickingIntervalID);
});
htmx.on("gameOver", () => {
  clearInterval(turnTickingIntervalID);
});
